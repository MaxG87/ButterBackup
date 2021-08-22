from __future__ import annotations

import contextlib
import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory

from butter_backup import shell_interface as sh


@contextlib.contextmanager
def decrypted_device(device: Path, map_name: str, pass_cmd: str):
    decrypt_cmd: sh.StrPathList = ["sudo", "cryptsetup", "open", device, map_name]
    close_cmd = ["sudo", "cryptsetup", "close", map_name]
    empty_env: dict[str, str] = {}  # make mypy happy
    pwd_proc = subprocess.run(pass_cmd, stdout=subprocess.PIPE, shell=True, check=True)
    subprocess.run(decrypt_cmd, input=pwd_proc.stdout, check=True, env=empty_env)
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


@contextlib.contextmanager
def symbolic_link(src: Path, dest: Path):
    """Create an symbolic link from `src` to `dest`

    This context manager will create a symbolic link from src to dest. It
    differentiates itself from `Path.link_to()` by …:

        * … creating the link with root privileges. This allows to limit root
          permissions to only the necessary parts of the program.

        * ensuring that the link gets removed after usage.

    Parameters:
    -----------
    src: Path to source; can be anything that has a filesystem path
    dest: Path to destination file

    Returns:
    --------
    The value of `dest.absolute()` will be returned.
    """

    if not src.exists():
        raise FileNotFoundError
    if dest.exists():
        raise FileExistsError
    absolute_dest = dest.absolute()
    sh.run_cmd(cmd=["sudo", "ln", "-s", f"{src.absolute()}", f"{absolute_dest}"])
    yield absolute_dest
    sh.run_cmd(cmd=["sudo", "rm", f"{absolute_dest}"])


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
