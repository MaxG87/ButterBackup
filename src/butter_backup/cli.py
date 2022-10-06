#!/usr/bin/env python3
import enum
import json
import os
import sys
from pathlib import Path
from tempfile import mkdtemp
from typing import Any, Callable, Optional

import typer
from loguru import logger

from . import __version__
from . import backup_backends as bb
from . import config_parser as cp
from . import device_managers as dm

app = typer.Typer()
DEFAULT_CONFIG_DIR = Path("~/.config/").expanduser()
DEFAULT_CONFIG_NAME = "butter-backup.cfg"


class ValidBackends(enum.Enum):
    restic = "restic"
    btrfs_rsync = "btrfs-rsync"


def get_default_config_path() -> str:
    config_dir = Path(os.getenv("XDG_CONFIG_HOME", DEFAULT_CONFIG_DIR))
    config_file = config_dir / DEFAULT_CONFIG_NAME
    return str(config_file)


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


CONFIG_OPTION = typer.Option(get_default_config_path(), exists=True, dir_okay=False)
VERBOSITY_OPTION = typer.Option(0, "--verbose", "-v", count=True)


@app.command()
def open(config: Path = CONFIG_OPTION, verbose: int = VERBOSITY_OPTION):
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
    """
    setup_logging(verbose)
    configurations = cp.parse_configuration(config.read_text())
    for cfg in configurations:
        if cfg.device().exists():
            mount_dir = Path(mkdtemp())
            decrypted = dm.open_encrypted_device(cfg.device(), cfg.DevicePassCmd)
            dm.mount_btrfs_device(
                decrypted, mount_dir=mount_dir, compression=cfg.Compression
            )
            typer.echo(f"Speichermedium {cfg.UUID} wurde in {mount_dir} geöffnet.")


@app.command()
def close(config: Path = CONFIG_OPTION, verbose: int = VERBOSITY_OPTION):
    """
    Schließe alle geöffneten Speichermedien

    Das Kommando `close` schließt alle gemounteten Speichermedien, deren UUIDs
    in der Konfiguration erwähnt werden. Es ist das Gegenstück des Kommandos
    `open`. Weitere Erklärungen finden sich dort.
    """
    setup_logging(verbose)
    configurations = cp.parse_configuration(config.read_text())
    mounted_devices = dm.get_mounted_devices()
    for cfg in configurations:
        mapped_device = f"/dev/mapper/{cfg.UUID}"
        if cfg.device().exists() and mapped_device in mounted_devices:
            mount_dirs = mounted_devices[mapped_device]
            if len(mount_dirs) != 1:
                # TODO introduce custom exception
                raise ValueError(
                    "Got several possible mount points. Expected exactly 1!"
                )
            mount_dir = mount_dirs.pop()
            dm.unmount_device(mount_dir)
            dm.close_decrypted_device(Path(mapped_device))
            mount_dir.rmdir()


@app.command()
def backup(config: Path = CONFIG_OPTION, verbose: int = VERBOSITY_OPTION):
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
    configurations = cp.parse_configuration(config.read_text())
    for cfg in configurations:
        if not cfg.device().exists():
            logger.info(
                f"Speichermedium {cfg.UUID} existiert nicht. Es wird kein Backup angelegt."
            )
            continue
        backend = bb.BackupBackend.from_config(cfg)
        with dm.decrypted_device(cfg.device(), cfg.DevicePassCmd) as decrypted:
            with dm.mounted_device(decrypted, cfg.Compression) as mount_dir:
                backend.do_backup(mount_dir)


@app.command()
def format_device(
    backend: ValidBackends = typer.Argument(...),  # noqa: B008
    device: Path = typer.Argument(  # noqa: B008
        ..., exists=True, dir_okay=False, readable=False
    ),
    config_to: Optional[Path] = typer.Option(  # noqa: B008
        None,
        help="Datei, in welche die generierte Konfiguration geschrieben werden"
        " soll. Die angegebene Datei darf nicht existieren. Wenn nicht"
        " angegeben, wird die Konfiguration auf STDOUT ausgegeben.",
    ),
    verbose: int = VERBOSITY_OPTION,
):
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
    `butter-backup` zusammen mit dem Passwortmanager `pass`.
    """
    setup_logging(verbose)
    config_writer: Callable[[str], Any]
    if config_to is None:
        config_writer = typer.echo
    else:
        if config_to.exists():
            raise ValueError(
                "Zieldatei für ButterBackup-Konfiguration existiert schon!"
            )
        config_writer = config_to.write_text
    formatter = (
        dm.prepare_device_for_butterbackend
        if backend == ValidBackends.btrfs_rsync
        else dm.prepare_device_for_resticbackend
    )
    config = formatter(device)
    json_serialisable = json.loads(config.json(exclude_none=True))
    config_writer(json.dumps([json_serialisable], indent=4, sort_keys=True))


@app.command()
def version():
    """Gibt butter-backups aktuelle Version an"""
    typer.echo(__version__)


def cli() -> None:
    app()


if __name__ == "__main__":
    cli()
