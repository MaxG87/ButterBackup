from __future__ import annotations

import subprocess
from dataclasses import dataclass
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Optional

from butter_backup import shell_interface as sh


@dataclass(frozen=True)
class DecryptedDevice:
    device: Path
    map_name: str
    pass_cmd: str

    def __enter__(self) -> Path:
        decrypt_cmd = f"sudo cryptsetup open '{self.device}' '{self.map_name}'"
        subprocess.run(f"{self.pass_cmd} | {decrypt_cmd}", check=True, shell=True)
        return Path(f"/dev/mapper/{self.map_name}")

    def __exit__(self, exc, value, tb) -> None:
        decrypt_cmd = f"sudo cryptsetup close '{self.map_name}'"
        sh.run_cmd(cmd=decrypt_cmd)


@dataclass
class MountedDevice:
    device: Path
    mount_dir: Optional[TemporaryDirectory] = None

    def __enter__(self) -> Path:
        if is_mounted(self.device):
            unmount_device(self.device)
        self.mount_dir = TemporaryDirectory()
        mount_btrfs_device(self.device, Path(self.mount_dir.name))
        return Path(self.mount_dir.name)

    def __exit__(self, exc, value, tb) -> None:
        if self.mount_dir is None:
            return
        unmount_device(self.device)
        self.mount_dir.__exit__(exc, value, tb)


def mount_btrfs_device(device: Path, mount_dir: Path) -> None:
    sh.run_cmd(cmd=f"sudo mount -o compress=zlib '{device}' '{mount_dir}'")


def is_mounted(dest: Path) -> bool:
    return str(dest) in get_mounted_devices()


def get_mounted_devices() -> dict[str, Path]:
    raw_mounts = sh.run_cmd(cmd="mount", capture_output=True)
    mount_lines = raw_mounts.stdout.decode().splitlines()
    return {line.split()[0]: Path(line.split()[2]) for line in mount_lines}


def unmount_device(device: Path) -> None:
    sh.run_cmd(cmd=f"sudo umount '{device}'")
