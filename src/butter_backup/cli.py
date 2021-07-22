#!/usr/bin/env python3

import typer

app = typer.Typer()


@app.command()
def hilfe():
    typer.echo("Hilfe!")


if __name__ == "__main__":
    app()
