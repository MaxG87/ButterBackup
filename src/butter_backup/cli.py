#!/usr/bin/env python3

from pathlib import Path

import typer

app = typer.Typer()


@app.command()
def hilfe():
    typer.echo("Hilfe!")


@app.command()
def open(config: Path = typer.Option(None, exists=True)):
    typer.echo("Open!")


if __name__ == "__main__":
    app()
