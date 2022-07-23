#!/usr/bin/env python3
import enum
import os
import sys
from pathlib import Path
from tempfile import mkdtemp

import typer
from loguru import logger

from . import backup_logic as bl
from . import config_parser as cp
from . import device_managers as dm

app = typer.Typer()
DEFAULT_CONFIG_DIR = Path("~/.config/").expanduser()
DEFAULT_CONFIG_NAME = "butter-backup.cfg"


class ValidBackends(enum.Enum):
    restic = "restic"
    butter_backup = "butter-backup"


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


CONFIG_OPTION = typer.Option(
    get_default_config_path(), exists=True, dir_okay=False, readable=True
)
VERBOSITY_OPTION = typer.Option(0, "--verbose", "-v", count=True)


@app.command()
def open(config: Path = CONFIG_OPTION, verbose: int = VERBOSITY_OPTION):
    setup_logging(verbose)
    configurations = list(cp.load_configuration(config))
    for cfg in configurations:
        if cfg.device().exists():
            mount_dir = Path(mkdtemp())
            decrypted = dm.open_encrypted_device(cfg.device(), cfg.DevicePassCmd)
            dm.mount_btrfs_device(decrypted, mount_dir=mount_dir)
            typer.echo(f"Gerät {cfg.UUID} wurde in {mount_dir} geöffnet.")


@app.command()
def close(config: Path = CONFIG_OPTION, verbose: int = VERBOSITY_OPTION):
    setup_logging(verbose)
    configurations = list(cp.load_configuration(config))
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
    setup_logging(verbose)
    configurations = list(cp.load_configuration(config))
    for cfg in configurations:
        if not cfg.device().exists():
            logger.info(
                f"Gerät {cfg.UUID} existiert nicht. Es wird kein Backup angelegt."
            )
            continue
        with dm.decrypted_device(cfg.device(), cfg.DevicePassCmd) as decrypted:
            with dm.mounted_device(decrypted) as mount_dir:
                bl.do_backup(cfg, mount_dir)


@app.command()
def format_device(
    device: Path = typer.Argument(..., exists=True, dir_okay=False),  # noqa: B008
    backend: ValidBackends = typer.Argument(...),  # noqa: B008
    verbose: int = VERBOSITY_OPTION,
):
    setup_logging(verbose)
    formatter = (
        dm.prepare_device_for_butterbackend
        if backend == ValidBackends.butter_backup
        else dm.prepare_device_for_resticbackend
    )
    config = formatter(device)
    typer.echo(f"[{config.json()}]")


def cli() -> None:
    app()


if __name__ == "__main__":
    cli()
