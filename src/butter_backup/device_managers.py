from __future__ import annotations

import contextlib
import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory

from butter_backup import shell_interface as sh


@contextlib.contextmanager
def decrypted_device(device: Path, map_name: str, pass_cmd: str):
    decrypt_cmd = f"sudo cryptsetup open '{device}' '{map_name}'"
    close_cmd = ["sudo", "cryptsetup", "close", map_name]
    subprocess.run(f"{pass_cmd} | {decrypt_cmd}", check=True, shell=True)
    yield Path(f"/dev/mapper/{map_name}")
    sh.run_cmd(cmd=close_cmd)


@contextlib.contextmanager
def mounted_device(device: Path):
    if is_mounted(device):
        unmount_device(device)
    with TemporaryDirectory() as td:
        mount_dir = Path(td)
        mount_btrfs_device(device, Path(mount_dir))
        yield Path(mount_dir)
        unmount_device(device)


def mount_btrfs_device(device: Path, mount_dir: Path) -> None:
    cmd: sh.StrPathList = [
        "sudo",
        "mount",
        "-o",
        "compress=zlib",
        device,
        mount_dir,
    ]
    sh.run_cmd(cmd=cmd)


def is_mounted(dest: Path) -> bool:
    return str(dest) in get_mounted_devices()


def get_mounted_devices() -> dict[str, Path]:
    raw_mounts = sh.run_cmd(cmd=["mount"], capture_output=True)
    mount_lines = raw_mounts.stdout.decode().splitlines()
    return {line.split()[0]: Path(line.split()[2]) for line in mount_lines}


def unmount_device(device: Path) -> None:
    cmd: sh.StrPathList = ["sudo", "umount", device]
    sh.run_cmd(cmd=cmd)
