# ButterBackup - Inkrementelle Vollbackups dank BtrFS

[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)
[![Imports: isort](https://img.shields.io/badge/%20imports-isort-%231674b1?style=flat&labelColor=ef8336)](https://pycqa.github.io/isort/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

## Installation

    poetry build
    pipx install dist/butter-backup-<version>.whl

## Ausführung der Tests

### Schnell

    poetry run flake8
    poetry run mypy .
    poetry run pytest

### Cross-Platform-Tests

Es existiert eine umfassende Testsuite, die auch gewisse
Plattformabhängigkeiten erkennen kann. Die Docker-Tests existieren überhaupt
nur, weil es Probleme mit Arch gab.

Die Testsuite kann mittels `make -j1` ausgeführt werden. **WICHTIG:** Die
Testsuite kann nicht parallelisiert ausgeführt werden. Eine parallelisierte
Ausführung lässt einige Mount-Tests fehlschlagen.

## Ähnliche Projekte

  * https://digint.ch/btrbk/

## Beispiele

## Ausstehende Aufgaben

  * README schreiben und übersetzen
  * Logo erstellen
  * Tests der Backup-Logik
  * Struktur umgestalten hin zu `BackupDirective`, die von Datei- oder
    Ordnersicherungen abstrahieren.
  * ungenutzte Ordner entfernen
  * `format-device` implementieren
  * Testsuite umstellen von Docker auf virtuelle Maschinen
  * Testsuite parallelisiert ausführen lassen

