[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)
[![Imports: isort](https://img.shields.io/badge/%20imports-isort-%231674b1?style=flat&labelColor=ef8336)](https://pycqa.github.io/isort/)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

<img src="logo.png" alt="ButterBackup-Logo" height="150px" align="right">

# ButterBackup - Backups so geschmeidig wie Butter!

**Inkrementelle Vollbackups dank BtrFS**

## Installation

    poetry build
    pipx install dist/butter-backup-<version>.whl

## Benutzung

[![asciicast](https://asciinema.org/a/HTFzRxEWw8ltCoP6NDNo6tITP.svg)](https://asciinema.org/a/HTFzRxEWw8ltCoP6NDNo6tITP)

## Gefährdungsszenario

ButterBackup wurde entworfen, um gegen eine Reihe spezieller
Gefährdungsszenarien zu schützen. Die konkreten Bedrohungen und die
entsprechenden Gegenmaßnahmen gibt die folgende Tabelle an. Auf die
Übersichtstabelle folgt eine etwas ausführlichere Diskussion der Maßnahmen.

| Bedrohung                                          | Gegenmaßnahme                        |
| -------------------------------------------------- | ------------------------------------ |
| Dateien verschlüsselnde Schadsoftware              | physikalisch getrennte Aufbewahrung  |
| fehlerhafte Sicherungskopien durch Fehlbenutzung   | sehr einfache Benutzung              |
| Verlust des Datenträgers                           | Vollverschlüsselung des Datenträgers |
| Zerstörung des Datenträgers durch Spannungsspitzen | physikalisch getrennte Aufbewahrung  |
| zu seltene Sicherungskopien                        | sehr einfache Benutzung              |

Diese Bedrohungen werden dadurch reduziert, dass eine sehr einfache Benutzung
die physikalisch getrennte Aufbewahrung des die Sicherungskopien enthaltenen
Datenträgers ermöglicht.

Eine einfach umzusetzende und verlässliche Maßnahme, um den Datenträger
vor Zerstörung durch Spannungsspitzen zu schützen, ist, ihn vom Computer
physikalisch getrennt aufzubewahren. Gleichzeitig schützt diese Maßnahme
auch sehr gut vor der Zerstörung der Sicherungskopien durch Schadsoftware, da
sich die Sicherungskopien außerhalb des Zugriffs der Schadsoftware befinden.

Durch die physikalisch getrennte Aufbewahrung ergibt sich aber eine Bedrohung
durch Bequemlichkeit. Ein Prozess, für dessen Durchführung manuelle
Schritte nötig sind, ist fehleranfällig und läuft Gefahr, im Zweifel
nicht ausgeführt zu werden. Das Anlegen von Sicherungskopien stellt hier
keine Ausnahme dar.

ButterBackup begegnet dieser Gefahr dadurch, dass es das Anlegen einer
Sicherungskopie auf zwei manuelle Schritte reduziert. Es genügt, die
Festplatte mit dem Computer zu verbinden und das Programm zu starten. Alle
weiteren Schritte, z.B. Entschlüsselung, Kopie der Daten und Unmounten,
werden von ButterBackup übernommen.

Insgesamt ermöglicht ButterBackup, Sicherungskopien so sicher wie möglich
aufzubewahren ohne dabei zu verkomplizieren, neue Sicherungskopien anzulegen.

## Ausführung der Tests

ButterBackup hat eine umfangreiche Testsuite, die viele relevanten Aspekte des
Programms abdeckt. Diese wird ergänzt durch die ähnlich umfangreichen
Testsuites der Abhängigkeiten `storage-device-managers` und `shell-interface`.
Diese beiden Abhängigkeiten sind aus ButterBackup hervorgegangen, womit deren
Testsuite weitere für ButterBackup wichtige Aspekte gründlich abdeckt.

Für die schnelle Ausführung während der Entwicklung bietet es sich an, die
Testsuite direkt auszuführen. Dies ist wird im Abschnitt "schnell" erläutert.
Spätestens vor einem Release sollten aber auch die distributionsübergreifenden
Tests ausgeführt werden, da diese garantieren, dass ButterBackup auch unter
Arch und allen unterstützten Pythonversionen funktioniert.


### Schnell

#### Direkt

    poetry run ruff check .
    poetry run mypy .
    poetry run pytest

#### Mittels Makefile

    make check-format check-linters run-undockered-tests

### Plattformübergreifende Tests

Es existiert eine umfassende Testsuite, die auch gewisse
Plattformabhängigkeiten erkennen kann. Die Docker-Tests existieren überhaupt
nur, weil es Probleme mit Arch gab.

Die Testsuite kann mittels `make` ausgeführt werden. Es ist auch eine
Parallelisierung mittels `make -j N` möglich, wobei N die Anzahl der Prozesse
angibt.

Diese Tests werden ergänzt durch eine Testmatrix auf Github. Diese Testmatrix
deckt Ubuntu mit allen unterstützten Pythonversionen ab. Dies erlaubt ggf., die
sehr lang laufende Docker-Testsuite seltener auszuführen.

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

## Ausstehende Aufgaben

- README schreiben und übersetzen
- Testsuite umstellen von Docker auf virtuelle Maschinen
- Alias für Einzelkonfigurationen?
- butter-backup exec / run
  - nimmt Befehl als Zeichenkette und führt diesen im BackupRootDir aus
  - butter-backup exec [<uuid>] <cmd> --> open; cd; cmd; cd -; close
  - sollte Mapping auf Umgebungsvariablen unterstützen, z.B. RepoPassCmd -> RESTIC_PASSWORD_COMMAND
- Verbesserte Fehlermeldungen
  - wenn unmount nicht möglich ist
  - wenn BackupRepository nicht vorhanden ist
- SudoPassCmd
- Konfiguration der gewünschten Kompression ermöglichen (zlib, zstd, none, etc.)
