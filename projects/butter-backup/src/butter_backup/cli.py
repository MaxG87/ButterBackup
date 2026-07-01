#!/usr/bin/env python3
import contextlib
import enum
import json
import os
import sys
import typing as t
from pathlib import Path
from tempfile import mkdtemp
from typing import Any, Callable

import shell_interface as sh
import storage_device_managers as sdm
import typer
from loguru import logger

from . import __version__
from . import backup_backends as bb
from . import config_parser as cp
from .device_managers import (
    prepare_device_for_butterbackend,
    prepare_device_for_resticbackend,
)

app = typer.Typer()
DEFAULT_CONFIG_DIR = Path("~/.config/").expanduser()
DEFAULT_CONFIG_BASENAME = "butter-backup"
DEFAULT_CONFIG_SUFFIX_ORDER = [".json5", ".json", ".toml", ".yaml"]
SUPPORTED_CONFIG_SUFFIXES = frozenset(DEFAULT_CONFIG_SUFFIX_ORDER)


class ValidBackends(enum.Enum):
    restic = "restic"
    btrfs_rsync = "btrfs-rsync"


class ValidFileSystems(enum.Enum):
    btrfs = "btrfs"
    ext4 = "ext4"


def get_default_config_paths() -> list[Path]:
    config_dir = Path(os.getenv("XDG_CONFIG_HOME", DEFAULT_CONFIG_DIR))
    return [
        config_dir / DEFAULT_CONFIG_BASENAME / f"config{suffix}"
        for suffix in DEFAULT_CONFIG_SUFFIX_ORDER
    ]


def _read_configuration(config: Path | None) -> cp.Configuration:
    if config is not None:
        if config.suffix not in SUPPORTED_CONFIG_SUFFIXES:
            raise typer.BadParameter(
                "Konfigurationsdatei muss auf .json, .json5, .toml oder .yaml enden.",
                param_hint="'--config'",
            )
        return cp.parse_configuration_by_extension(config.read_text(), config.suffix)

    for maybe_config in get_default_config_paths():
        if maybe_config.exists():
            return cp.parse_configuration_by_extension(
                maybe_config.read_text(), maybe_config.suffix
            )

    tried = ", ".join(str(path) for path in get_default_config_paths())
    typer.echo(
        f"Keine Konfigurationsdatei gefunden. Es wurden folgende Pfade geprüft: {tried}",
        err=True,
    )
    raise typer.Exit(2)


def setup_logging(verbosity: int) -> None:
    # If no `-v/--verbose` is given (verbosity == 0 in this case), errors and
    # warnings shall appear. The first flag shall let successes appear, so that
    # the user can trace the progress of the program.
    logger.remove()
    available_levels = [
        # "CRITICAL",
        # "ERROR",
        "WARNING",
        "SUCCESS",
        "INFO",
        "DEBUG",
        "TRACE",
    ]
    level = min(verbosity, len(available_levels) - 1)
    logger.add(sys.stderr, level=available_levels[level])


def _get_default_file_system(backend: ValidBackends) -> ValidFileSystems:
    match backend:
        case ValidBackends.btrfs_rsync:
            return ValidFileSystems.btrfs
        case ValidBackends.restic:
            return ValidFileSystems.ext4
        case _:
            t.assert_never(backend)


def _refresh_sudo(sudo_pass_cmd: str | None) -> None:
    if sudo_pass_cmd is not None:
        sh.pipe_pass_cmd_to_real_cmd(
            sudo_pass_cmd, ["sudo", "-Sv"], capture_output=True
        )


def _skip_device(
    config: cp.DeviceConfiguration,
    *,
    log_missing: Callable[[cp.DeviceConfiguration], None] | None = None,
    log_opened: Callable[[cp.DeviceConfiguration], None] | None = None,
) -> bool:
    """
    Helper function to determine whether a device should be skipped.

    A device should be skipped if:
      - it is not present in the system
      - it is already opened by cryptsetup

    The first condition is obvious. The second condition is important to prevent subtle
    race conditions when a user already works on the device. In this case she e.g. might
    remove it before a started backup is finished.
    """
    if not config.device().exists():
        if log_missing is not None:
            log_missing(config)
        return True
    if config.map_name().exists():
        if log_opened is not None:
            log_opened(config)
        return True
    return False


CONFIG_OPTION = typer.Option(None, exists=True, dir_okay=False)
VERBOSITY_OPTION = typer.Option(0, "--verbose", "-v", count=True)


def _open_device(
    cfg: cp.DeviceConfiguration, base_dir: Path, sudo_pass_cmd: str | None
) -> None:
    mount_dir = base_dir / cfg.Name
    created_mount_dir = False
    try:
        _refresh_sudo(sudo_pass_cmd)
        created_mount_dir = sdm.ensure_directory(mount_dir)
        decrypted = sdm.open_encrypted_device(cfg.device(), cfg.DevicePassCmd)
        sdm.mount_device(decrypted, mount_dir=mount_dir, compression=cfg.compression())
    except Exception:
        # In case of **any** error, the mount dir should be removed to prevent littering
        # the file system with empty directories. Hence the pokemon style exception
        # handling.
        typer.echo(
            f"Speichermedium {cfg.Name} konnte nicht geöffnet werden. Es wird übersprungen."
        )
        if created_mount_dir:
            with contextlib.suppress(sh.ShellInterfaceError):
                sh.run_cmd(cmd=["sudo", "rmdir", mount_dir])
    else:
        typer.echo(f"Speichermedium {cfg.Name} wurde in {mount_dir} geöffnet.")


@app.command()
def open(  # noqa: A001
    config: Path | None = CONFIG_OPTION,
    verbose: int = VERBOSITY_OPTION,
) -> None:
    """
    Öffne alle in der Konfiguration gelisteten Speichermedien

    Das Kommando `open` öffnet alle Speichermedien, deren UUID in der
    Konfiguration erwähnt wird.  Für jedes geöffnete Speichermedium wird die
    UUID und der Mount-Zielordner angegeben.

    Dies Kommando ist besonders nützlich um Sicherheitskopien
    wiederherzustellen. Dafür wird das Speichermedium, auf dem sich die
    Sicherheitskopien befinden, mittels `butter-backup open` geöffnet. Dann
    kann mit den Daten interagiert werden, z.B. durch Öffnen im Dateibrowser
    oder durch Verwendung von `restic`. Nach erfolgreicher Wiederherstellung
    kann das Speichermedium mit `butter-backup close` wieder entfernt werden.

    Das Zielverzeichnis für die Speichermedien kann in der Konfiguration über
    das Feld `OpenDirectory` festgelegt werden. Wenn nicht angegeben, wird ein
    temporäres Verzeichnis erstellt.
    """
    setup_logging(verbose)
    parsed_config = _read_configuration(config)
    open_dir = parsed_config.OpenDirectory
    base_dir = open_dir if open_dir is not None else Path(mkdtemp())
    for cfg in parsed_config.DeviceConfigurations:
        if _skip_device(
            cfg,
            log_opened=lambda cfg: logger.warning(
                f"Speichermedium {cfg.Name} ist bereits geöffnet. Es wird übersprungen."
            ),
        ):
            continue
        _open_device(cfg, base_dir, parsed_config.SudoPassCmd)


@app.command()
def close(config: Path | None = CONFIG_OPTION, verbose: int = VERBOSITY_OPTION) -> None:
    """
    Schließe alle geöffneten Speichermedien

    Das Kommando `close` schließt alle gemounteten Speichermedien, deren UUIDs
    in der Konfiguration erwähnt werden. Es ist das Gegenstück des Kommandos
    `open`. Weitere Erklärungen finden sich dort.
    """
    setup_logging(verbose)
    parsed_config = _read_configuration(config)
    mounted_devices = sdm.get_mounted_devices()
    for cfg in parsed_config.DeviceConfigurations:
        map_name = cfg.map_name()
        map_name_as_str = str(map_name)
        if cfg.device().exists() and map_name_as_str in mounted_devices:
            mount_dirs = mounted_devices[map_name_as_str]
            num_mount_dirs = len(mount_dirs)
            if num_mount_dirs != 1:
                logger.error(
                    "Got {num_mount_dirs} mount points for device {device}. Expected"
                    " exactly 1! Skipping device.",
                    num_mount_dirs=num_mount_dirs,
                    device=cfg.Name,
                )
                continue
            _refresh_sudo(parsed_config.SudoPassCmd)
            sdm.unmount_device(map_name)
            sdm.close_decrypted_device(map_name)


@app.command()
def backup(
    config: Path | None = CONFIG_OPTION, verbose: int = VERBOSITY_OPTION
) -> None:
    """
    Führe Sicherheitskopien durch

    Für jedes angeschlossene Speichermedium wird eine Sicherheitskopie gemäß
    der Konfiguration durchgeführt. Das Speichermedium wird hierfür
    entschlüsselt und gemountet. Nachdem die Sicherungskopie durchgeführt
    wurde, wird es wieder vollständig geschlossen.

    Die Sicherheitskopien werden sequentiell durchgeführt. Dadurch wird
    sichergestellt, dass auch auf HDDs brauchbare Lesegeschwindigkeiten erzielt
    werden können.

    Direkt nach Beendigung der Durchführung der Sicherheitskopien, d.h. nachdem
    `butter-backup backup` zurückgekehrt ist oder nachdem die Durchführung der
    Sicherheitskopien des nächsten Speichermediums begonnen wurde, kann das
    entsprechende Speichermedium physisch entfernt werden. Eine Wartezeit oder
    weitere manuelle Schritte sind nicht nötig.
    """
    setup_logging(verbose)
    parsed_config = _read_configuration(config)
    for cfg in parsed_config.DeviceConfigurations:
        if _skip_device(
            cfg,
            log_missing=lambda cfg: logger.info(
                f"Speichermedium {cfg.Name} existiert nicht. Es wird kein Backup angelegt."
            ),
            log_opened=lambda cfg: logger.warning(
                f"Speichermedium {cfg.Name} ist bereits geöffnet. Es wird übersprungen."
            ),
        ):
            continue
        backend = bb.BackupBackend.from_config(cfg)
        _refresh_sudo(parsed_config.SudoPassCmd)
        open_dir = parsed_config.OpenDirectory
        dest = open_dir / cfg.Name if open_dir is not None else None
        with (
            sdm.decrypted_device(cfg.device(), cfg.DevicePassCmd) as decrypted,
            sdm.mounted_device(decrypted, dest, compression=cfg.compression()) as mount_dir,
        ):
            backend.do_backup(mount_dir, parsed_config.SudoPassCmd)
            # A backup could take so long that the sudo session expires. In this
            # case the user would have to enter the password again to unmount and
            # close the device. To prevent this, the sudo session is refreshed.
            _refresh_sudo(parsed_config.SudoPassCmd)


@app.command()
def format_device(
    backend: ValidBackends = typer.Argument(...),  # noqa: B008
    device: Path = typer.Argument(  # noqa: B008
        ..., exists=True, dir_okay=False, readable=False
    ),
    file_system: ValidFileSystems | None = typer.Option(  # noqa: B008
        None,
        "--file-system",
        help="Dateisystem für das Restic-Backend. Andere Werte als `btrfs` nur für das"
        "Restic-Backend gültig. Unterstützte Dateisysteme: btrfs, ext4.",
    ),
    config_to: Path | None = typer.Option(  # noqa: B008
        None,
        help="Datei, in welche die generierte Konfiguration geschrieben werden"
        " soll. Die angegebene Datei darf nicht existieren. Wenn nicht"
        " angegeben, wird die Konfiguration auf STDOUT ausgegeben.",
    ),
    verbose: int = VERBOSITY_OPTION,
) -> None:
    """
    Richtet Speichermedium für butter-backup ein

    Das angegebene Speichermedium wird vollständig zur Erstellung von
    Sicherheitskopien mit `butter-backup` vorbereitet. Es wird eine
    Konfiguration ausgegeben, die nur noch um die zu sichernden Ordner bzw.
    Dateien ergänzt werden muss.

    Die in der ausgegebenen Konfiguration enthaltenen Passwörter werden mittels
    kryptographisch sicheren Methoden erstellt.

    Es wird dringend angeraten, die Passwörter nicht in der Konfiguration zu
    belassen, sondern in einen Passwortmanager zu tun. Der Autor verwendet
    `butter-backup` zusammen mit dem Passwortmanager `pass` und 1Passwords
    Kommandozeilenprogramm.
    """
    setup_logging(verbose)
    file_system = file_system or _get_default_file_system(backend)

    if file_system != ValidFileSystems.btrfs and backend != ValidBackends.restic:
        raise typer.BadParameter(
            "Andere Dateisysteme als BtrFS sind nur für das Restic-Backend gültig.",
            param_hint="'--file-system'",
        )
    config_writer: Callable[[str], Any]
    if config_to is None:
        config_writer = typer.echo
    else:
        if config_to.exists():
            raise ValueError(
                "Zieldatei für ButterBackup-Konfiguration existiert schon!"
            )
        config_writer = config_to.write_text
    config: cp.DeviceConfiguration
    match backend:
        case ValidBackends.btrfs_rsync:
            config = prepare_device_for_butterbackend(device)
        case ValidBackends.restic:
            config = prepare_device_for_resticbackend(device, file_system.value)
        case _:
            t.assert_never(backend)
    json_serialisable = json.loads(config.model_dump_json(exclude_none=True))
    wrapper = {"DeviceConfigurations": [json_serialisable]}
    config_writer(json.dumps(wrapper, indent=4, sort_keys=True))


@app.command()
def version() -> None:
    """Gibt butter-backups aktuelle Version an"""
    typer.echo(__version__)


def cli() -> None:
    app()


if __name__ == "__main__":
    cli()
