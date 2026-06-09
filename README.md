[![Ruff](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/astral-sh/ruff/main/assets/badge/v2.json)](https://github.com/astral-sh/ruff)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

<img src="projects/butter-backup/logo.png" alt="ButterBackup-Logo" height="150px" align="right">

# ButterBackup – Backups so geschmeidig wie Butter!

📖 [🇬🇧 English version](docs/README.en.md)

---

Dieses Repository enthält drei eng verwandte Python-Projekte, die gemeinsam
entwickelt werden.

## Projekte

### 🧈 [ButterBackup](projects/butter-backup/)

Vollverschlüsselte, pseudoinkrementelle Sicherungskopien leicht gemacht.
ButterBackup ist ein Kommandozeilenwerkzeug, das das Anlegen von
Sicherungskopien auf verschlüsselten externen Datenträgern so einfach wie
möglich macht.

```bash
pipx install butter-backup
butter-backup backup --config ~/.config/butter-backup/config.json5
```

➡️ [Zur vollständigen Dokumentation](projects/butter-backup/README.md)

---

### 💾 [Storage Device Managers](projects/storage-device-managers/)

Kontextmanager zum Entschlüsseln und Einbinden von Speichergeräten.
Unterstützt LUKS-Verschlüsselung, BtrFS- und ext4-Dateisysteme sowie
automatische Dateisystemerkennung.

➡️ [Zur vollständigen Dokumentation](projects/storage-device-managers/README.md)

---

### 🐚 [Shell Interface](projects/shell-interface/)

Leichtgewichtige Hilfsfunktionen zur Interaktion mit UNIX-Shells. Bietet
sicheres Ausführen von Shell-Kommandos, Piping und Abfrage von
Systeminformationen.

➡️ [Zur vollständigen Dokumentation](projects/shell-interface/README.md)

---

## Schnellstart für Entwickler

```bash
git clone https://github.com/MaxG87/ButterBackup.git
cd ButterBackup
uv sync
```
