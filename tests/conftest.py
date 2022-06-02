from __future__ import annotations

from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest

from butter_backup import device_managers as dm
from butter_backup import shell_interface as sh


@pytest.fixture
def mounted_directories():
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as mountpoint:
            sh.run_cmd(cmd=["sudo", "mount", "-o", "bind", src, mountpoint])
            yield Path(src), Path(mountpoint)
            sh.run_cmd(cmd=["sudo", "umount", mountpoint])


@pytest.fixture
def big_file():
    def get_random_filename(dir_: str) -> str:
        with NamedTemporaryFile(dir=dir_) as ntf:
            pass
        return ntf.name

    min_size = 128 * 1024**2  # ~109MiB is the minimum size for BtrFS
    with TemporaryDirectory() as tempdir:
        filename = get_random_filename(dir_=tempdir)
        file = Path(filename)
        with file.open("wb") as fh:
            fh.write(bytes(min_size))
        yield file


def _mkfs_btrfs(file: Path) -> None:
    cmd: sh.StrPathList = ["sudo", "mkfs.btrfs", file]
    sh.run_cmd(cmd=cmd)


@pytest.fixture
def btrfs_device(big_file: Path):
    _mkfs_btrfs(big_file)
    return big_file


@pytest.fixture
def encrypted_device(big_file: Path):
    _PassPhrase = "supersecure"
    dm.encrypt_device(big_file, _PassPhrase)
    yield _PassPhrase, big_file


@pytest.fixture
def encrypted_btrfs_device(encrypted_device):
    password, device = encrypted_device
    with dm.decrypted_device(device=device, pass_cmd=f"echo {password}") as dd:
        _mkfs_btrfs(dd)
    return encrypted_device
