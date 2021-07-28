#!/usr/bin/env python3
import os
import sys
from pathlib import Path
from typing import Optional

import typer

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
def open(config: Optional[Path] = CONFIG_OPTION):
    typer.echo("Open!")


@app.command()
def backup(
    config: Optional[Path] = CONFIG_OPTION
):
    typer.echo("Backup!")


if __name__ == "__main__":
    app()
