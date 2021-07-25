#!/usr/bin/env python3

import typer

app = typer.Typer()


@app.command()
def hilfe():
    typer.echo("Hilfe!")


@app.command()
def open():
    typer.echo("Open!")


if __name__ == "__main__":
    app()
