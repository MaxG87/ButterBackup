#!/usr/bin/env python3
import os
from pathlib import Path

import typer

from butter_backup import backup_logic as bl

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
    typer.echo("Unterbefehl `open` ist noch nicht implementiert.", err=True)
    raise typer.Exit(1)


@app.command()
def backup(config: Path = CONFIG_OPTION):
    bl.do_backup(config)


def cli() -> None:
    app()


if __name__ == "__main__":
    cli()
