[![Ruff](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/astral-sh/ruff/main/assets/badge/v2.json)](https://github.com/astral-sh/ruff)
[![Checked with mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

# Storage Device Managers - Helpful context managers for managing decryption and mounts of storage devices

## Overview

The `storage_device_managers` module provides a set of utilities to manage
encrypted storage devices, handle BtrFS and ext4 mounts, and perform file
system operations in a secure and structured way. It is designed to support
common storage-related tasks such as:

- Decrypting and mounting encrypted devices
- Managing BtrFS and ext4 mounts (with automatic file system detection)
- Creating and removing symbolic links with root privileges
- Formatting and encrypting devices (BtrFS and ext4)
- Changing file ownership securely

**Requires Python 3.11 or newer.**

## Features

- **Device Decryption & Encryption**: Easily decrypt and encrypt storage devices using `cryptsetup`. `PassCmdError` is raised when the password command fails, distinct from other shell errors.
- **BtrFS and ext4 Mount Management**: Mount and unmount BtrFS file systems with optional compression settings, or ext4 file systems.
- **Automatic File System Detection**: Use `get_filesystem` to detect a device's file system type, and `mount_device` to mount it without specifying the type manually.
- **Symbolic Link Handling**: Create and remove symbolic links with elevated permissions.
- **File System Operations**: Format devices with BtrFS or ext4 using dedicated functions or the unified `mkfs` helper, manage ownership, and check mount status.
- **Secure Passphrase Handling**: Automatically generate safe passwords for encryption.

## Usage

### Decrypting and Mounting a Device

```python
from pathlib import Path
from storage_device_managers import decrypted_device, mounted_device

# Decrypt and mount a device
with decrypted_device(Path("/dev/sdb1"), "cat /path/to/password-file") as dev:
    with mounted_device(dev) as mount_point:
        print(f"Device mounted at {mount_point}")
```

### Encrypting a Device

```python
from pathlib import Path
from storage_device_managers import encrypt_device

uuid = encrypt_device(Path("/dev/sdb1"), "cat /path/to/password-file")
print(f"Device encrypted with UUID: {uuid}")
```

### Creating a Symbolic Link

```python
from pathlib import Path
from storage_device_managers import symbolic_link

src = Path("/path/to/source")
dest = Path("/path/to/destination")

with symbolic_link(src, dest) as link:
    print(f"Symbolic link created at {link}")
```

## API Reference

### Context Managers

- `decrypted_device(device: Path, pass_cmd: str) -> Iterator[Path]`
  - Decrypts a device using `cryptsetup` and returns a context-managed path.
  - Raises `shell_interface.PassCmdError` if the password command fails.
  - Raises `shell_interface.ShellInterfaceError` on other shell-related errors.
- `mounted_device(device: Path, compression: ValidCompressions | None = None) -> Iterator[Path]`
  - Mounts a device to a temporary directory, auto-detecting the file system type. For BtrFS, optional compression settings are supported.
- `symbolic_link(src: Path, dest: Path) -> Iterator[Path]`
  - Creates and removes a symbolic link with root privileges.

### Utility Functions

- `get_filesystem(device: Path) -> str`
- `mount_device(device: Path, mount_dir: Path, compression: ValidCompressions | None = None) -> None`
- `mount_btrfs_device(device: Path, mount_dir: Path, compression: ValidCompressions | None = None) -> None`
- `mount_ext4_device(device: Path, mount_dir: Path) -> None`
- `is_mounted(device: Path) -> bool`
- `get_mounted_devices() -> Mapping[str, Mapping[Path, frozenset[str]]]`
- `unmount_device(device: Path) -> None`
- `open_encrypted_device(device: Path, pass_cmd: str) -> Path`
  - Raises `shell_interface.PassCmdError` if the password command fails.
  - Raises `shell_interface.ShellInterfaceError` on other shell-related errors.
- `close_decrypted_device(device: Path) -> None`
- `encrypt_device(device: Path, password_cmd: str) -> UUID`
  - Raises `shell_interface.PassCmdError` if the password command fails.
  - Raises `shell_interface.ShellInterfaceError` on other shell-related errors.
- `mkfs(device: Path, filesystem: ValidFileSystems) -> None`
- `mkfs_btrfs(device: Path) -> None`
- `mkfs_ext4(device: Path) -> None`
- `generate_passcmd() -> str`
- `chown(file_or_folder: Path, user: int | str, group: int | str | None = None, *, recursive: bool) -> None`

## Contributing

Contributions are welcome! Please submit issues and pull requests via GitHub.
