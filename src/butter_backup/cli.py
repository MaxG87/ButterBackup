#!/usr/bin/env python3
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


CONFIG_OPTION = typer.Option(get_default_config_path(), exists=True)
VERBOSITY_OPTION = typer.Option(0, "--verbose", "-v", count=True)


@app.command()
def hilfe():
    typer.echo("Hilfe!")


@app.command()
def open(config: Path = CONFIG_OPTION):
    configurations = list(cp.load_configuration(config))
    for cfg in configurations:
        if cfg.device().exists():
            mount_dir = Path(mkdtemp())
            decrypted = dm.open_encrypted_device(cfg.device(), cfg.DevicePassCmd)
            dm.mount_btrfs_device(decrypted, mount_dir=mount_dir)
            typer.echo(f"Gerät {cfg.UUID} wurde in {mount_dir} geöffnet.")


@app.command()
def close(config: Path = CONFIG_OPTION):
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
def backup(config: Path = CONFIG_OPTION):
    configurations = list(cp.load_configuration(config))
    for cfg in configurations:
        bl.do_backup(cfg)


def cli() -> None:
    app()


if __name__ == "__main__":
    cli()
