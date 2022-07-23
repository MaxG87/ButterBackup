# ButterBackup - Inkrementelle Vollbackups dank BtrFS

[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)
[![Imports: isort](https://img.shields.io/badge/%20imports-isort-%231674b1?style=flat&labelColor=ef8336)](https://pycqa.github.io/isort/)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

## Installation

    poetry build
    pipx install dist/butter-backup-<version>.whl

## Ausführung der Tests

**ACHTUNG**: Es gibt einen Konflikt mit Docker, der dazu führt, dass genau ein
Test der Testsuite bei der ersten Ausführung fehlschlägt. Ab der zweiten
Ausführung ist die Test-Suite aber robust und zuverlässig.

### Schnell

    poetry run flake8
    poetry run mypy .
    poetry run pytest

### Plattformübergreifende Tests

Es existiert eine umfassende Testsuite, die auch gewisse
Plattformabhängigkeiten erkennen kann. Die Docker-Tests existieren überhaupt
nur, weil es Probleme mit Arch gab.

Die Testsuite kann mittels `make` ausgeführt werden. Es ist auch eine
Paralellisierung mittels `make -j N` möglich, wobei N die Anzahl der Prozesse
angibt.

## Ähnliche Projekte

- https://digint.ch/btrbk/

## Designentscheidungen

### Beibehaltung von nicht mehr vermerkten Zielordnern und -dateien

In den Konfigurationen müssen Zielordner und Dateien angegeben werden. Nun ist
es denkbar, dass einige dieser Ziele umbenannt oder entfernt werden. Als
Beispiel diene hier das Backup des Ordners `Videos-Quelle` nach `Videos-Ziel`.
Ein Nutzer könnte sich nun entscheiden, aus Platzgründen `Videos-Quelle` auf
eine externe Festplatte auszulagern. Die Backup-Anweisung könnte damit entfernt
werden.

In diesem Fall wird ButterBackup `Videos-Ziel` nicht löschen. Der
offensichtlichste Grund ist, dass dies das Programm verkomplizieren würde. Es
ist ohne weiteres möglich, über `butter-backup open` die Sicherungskopien zu
öffnen und nicht mehr benötigte Ordner händisch zu löschen.

Außerdem ist es vorstellbar, dass die Quelle zwar gelöscht wurde, aber im
Backup archiviert werden soll. Dieser Anwendungsfall würde unmöglich gemacht,
würden nicht mehr gelistete Ziele aus der Sicherungskopie entfernt.

## Beispiele

## Ausstehende Aufgaben

- README schreiben und übersetzen
- Logo erstellen
- Testsuite umstellen von Docker auf virtuelle Maschinen
- RequiresRoot als globales Flag
- Alias für Einzelkonfigurationen?
- butter-backup exec / run
  * nimmt Befehl als Zeichenkette und führt diesen im BackupRootDir aus
  * butter-backup exec [<uuid>] <cmd>  --> open; cd; cmd; cd -; close
  * sollte Mapping auf Umgebungsvariablen unterstützen, z.B. RepoPassCmd -> RESTIC_PASSWORD_COMMAND
