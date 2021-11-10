#!/usr/bin/env python3
import os
from pathlib import Path
from tempfile import mkdtemp

import typer

from butter_backup import backup_logic as bl
from butter_backup import config_parser as cp
from butter_backup import device_managers as dm

app = typer.Typer()
DEFAULT_CONFIG_DIR = Path("~/.config/").expanduser()
DEFAULT_CONFIG_NAME = "butter-backup.cfg"


def get_default_config_path() -> str:
    config_dir = Path(os.getenv("XDG_CONFIG_HOME", DEFAULT_CONFIG_DIR))
    config_file = config_dir / DEFAULT_CONFIG_NAME
    return str(config_file)


CONFIG_OPTION = typer.Option(get_default_config_path(), exists=True)


@app.command()
def hilfe():
    typer.echo("Hilfe!")


@app.command()
def open(config: Path = CONFIG_OPTION):
    config_list = cp.load_configuration(config)
    for raw_cfg in config_list:
        parsed_cfg = cp.ParsedButterConfig.from_dict(raw_cfg)
        cfg = cp.ButterConfig.from_raw_config(parsed_cfg)
        if cfg.device().exists():
            mount_dir = Path(mkdtemp())
            decrypted = dm.open_encrypted_device(cfg.device(), cfg.pass_cmd)
            dm.mount_btrfs_device(decrypted, mount_dir=mount_dir)
            typer.echo(f"Gerät {cfg.uuid} wurde in {mount_dir} geöffnet.")


@app.command()
def close(config: Path = CONFIG_OPTION):
    config_list = cp.load_configuration(config)
    for raw_cfg in config_list:
        parsed_cfg = cp.ParsedButterConfig.from_dict(raw_cfg)
        cfg = cp.ButterConfig.from_raw_config(parsed_cfg)
        mapped_device = f"/dev/mapper/{cfg.uuid}"
        mounted_devices = dm.get_mounted_devices()
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
    bl.do_backup(config)


def cli() -> None:
    app()


if __name__ == "__main__":
    cli()
