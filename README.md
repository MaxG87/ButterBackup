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

### Plattformübergreifende Tests

Es existiert eine umfassende Testsuite, die auch gewisse
Plattformabhängigkeiten erkennen kann. Die Docker-Tests existieren überhaupt
nur, weil es Probleme mit Arch gab.

Die Testsuite kann mittels `make -j1` ausgeführt werden. **WICHTIG:** Die
Testsuite kann nicht parallelisiert ausgeführt werden. Eine parallelisierte
Ausführung lässt einige Mount-Tests fehlschlagen.

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
öffnen und den händisch zu löschen.

Außerdem ist es vorstellbar, dass die Quelle zwar gelöscht wurde, aber im
Backup archiviert werden soll. Dieser Anwendungsfall würde unmöglich gemacht,
würden nicht mehr gelistete Ziele aus der Sicherungskopie entfernt.

### Warum ist das Parsen der Konfigurationen so umständlich

Es ist in der Tat etwas unglücklich, dass so viele Zwischenschritte nötig sind,
um aus einer Konfigurationsdatei eine brauchbare Konfiguration zu erstellen.
Hier sind Verbesserungen wünschenswert.

Der Grund für die Kleinteiligkeit ist, dass dadurch Zuständigkeiten besser
aufgeteilt werden können. Die Klasse `ParsedButterConfig` verantwortet die
syntaktische Korrektheit der Konfiguration. Darunetr fällt beispielsweise, dass
genau die erwarteten Schlüssel angegeben werden. Die Klasse `ButterConfig` ist
für die semantische Korrektheit zuständig. Sie überprüft beispielsweise, dass
Quellen und Ziele nur jeweils einmal angegeben wurden, oder dass alle Quellen
auch existieren.

Es erscheint dem Autor nicht richtig, diese beiden Verantwortlichkeiten zu
mischen.

## Beispiele

## Ausstehende Aufgaben

- README schreiben und übersetzen
- Logo erstellen
- Tests der Backup-Logik
- Struktur umgestalten hin zu `BackupDirective`, die von Datei- oder
  Ordnersicherungen abstrahieren.
- `format-device` implementieren
- Testsuite umstellen von Docker auf virtuelle Maschinen
- Testsuite parallelisiert ausführen lassen