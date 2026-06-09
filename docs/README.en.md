[![Ruff](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/astral-sh/ruff/main/assets/badge/v2.json)](https://github.com/astral-sh/ruff)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

<img src="../projects/butter-backup/logo.png" alt="ButterBackup logo" height="150px" align="right">

# ButterBackup – Backups as smooth as butter!

📖 [🇩🇪 Deutsche Version](../README.md)

---

This repository contains three closely related Python projects developed
together as a workspace.

## Projects

### 🧈 [ButterBackup](../projects/butter-backup/)

Fully encrypted, pseudo-incremental backups made easy. ButterBackup is a
command-line tool that makes creating backups on encrypted external drives as
simple as possible.

```bash
pipx install butter-backup
butter-backup backup --config ~/.config/butter-backup/config.json5
```

➡️ [Full documentation](../projects/butter-backup/README.md)

---

### 💾 [Storage Device Managers](../projects/storage-device-managers/)

Context managers for decrypting and mounting storage devices. Supports LUKS
encryption, BtrFS and ext4 file systems, and automatic file system detection.

➡️ [Full documentation](../projects/storage-device-managers/README.md)

---

### 🐚 [Shell Interface](../projects/shell-interface/)

Lightweight utilities for interacting with UNIX shells. Provides safe command
execution, piping, and system information queries.

➡️ [Full documentation](../projects/shell-interface/README.md)

---

## Quickstart for developers

```bash
git clone https://github.com/MaxG87/ButterBackup.git
cd ButterBackup
uv sync
```
