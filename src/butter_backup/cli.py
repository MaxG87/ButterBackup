#!/usr/bin/env python3
import os
import sys
from pathlib import Path

import typer

from butter_backup import backup_logic as bl
from butter_backup import config_parser as cp

app = typer.Typer()
DEFAULT_CONFIG_DIR = Path("~/.config/").expanduser()
DEFAULT_CONFIG_NAME = Path("butter-backup.cfg")


def get_default_config_path() -> Path:
    config_dir = Path(os.getenv("XDG_CONFIG_HOME", DEFAULT_CONFIG_DIR))
    config_file = config_dir / DEFAULT_CONFIG_NAME
    if not config_file.exists():
        sys.exit(f"Konfigurationsdatei {config_file} existiert nicht.")
    return config_file


CONFIG_OPTION = typer.Option(get_default_config_path(), exists=True)


@app.command()
def hilfe():
    typer.echo("Hilfe!")


@app.command()
def open(config: Path = CONFIG_OPTION):
    config_list = cp.load_configuration(config)
    for raw_cfg in config_list:
        parsed_cfg = cp.ParsedButterConfig.from_dict(raw_cfg)
        # cfg = cp.ButterConfig.from_raw_config(parsed_cfg)
        typer.echo(f"Gerät {parsed_cfg.uuid} wurde in /tmp/does-not-exist geöffnet.")


@app.command()
def backup(config: Path = CONFIG_OPTION):
    bl.do_backup(config)


def cli() -> None:
    app()


if __name__ == "__main__":
    cli()
