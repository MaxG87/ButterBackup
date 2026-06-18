from __future__ import annotations

import shutil
import typing as t
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
import shell_interface as sh

import storage_device_managers as sdm


def get_random_filename(dir_: str) -> str:
    with NamedTemporaryFile(dir=dir_) as ntf:
        pass
    return ntf.name


@pytest.fixture(params=["btrfs_device", "ext4_device"])
def device_with_fs(request) -> tuple[Path, sdm.ValidFileSystems]:
    filesystem = request.param.split("_")[0]
    return (request.getfixturevalue(request.param), filesystem)


@pytest.fixture
def mounted_directories():
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as mountpoint:
            sh.run_cmd(cmd=["sudo", "mount", "-o", "bind", src, mountpoint])
            yield Path(src), Path(mountpoint)
            sh.run_cmd(cmd=["sudo", "umount", mountpoint])


@pytest.fixture(scope="session")
def _big_file_persistent():
    min_size = 128 * 1024**2  # ~109MiB is the minimum size for BtrFS
    with TemporaryDirectory() as tempdir:
        filename = get_random_filename(dir_=tempdir)
        file = Path(filename)
        with file.open("wb") as fh:
            fh.write(bytes(min_size))
        yield file


@pytest.fixture
def big_file(_big_file_persistent: Path):
    with NamedTemporaryFile() as ntf:
        file = Path(ntf.name)
        shutil.copy(_big_file_persistent, file)
        yield file


@pytest.fixture(scope="session", params=t.get_args(sdm.ValidFileSystems))
def _encrypted_device_persistent(_big_file_persistent, request):
    file_system: sdm.ValidFileSystems = request.param
    password_cmd = sdm.generate_passcmd()
    with NamedTemporaryFile() as ntf:
        device = Path(ntf.name)
        shutil.copy(_big_file_persistent, device)
        sdm.encrypt_device(device, password_cmd)
        with sdm.decrypted_device(device, password_cmd) as decrypted:
            sdm.mkfs(decrypted, file_system)
        yield device, password_cmd


@pytest.fixture
def encrypted_device(_encrypted_device_persistent):
    """
    Create encrypted file system

    Returns
    -------
    Path
        destination of encrypted file system
    str
        password command that echos password to STDOUT
    """
    btrfs_device, pass_cmd = _encrypted_device_persistent
    with NamedTemporaryFile() as ntf:
        file = Path(ntf.name)
        shutil.copy(btrfs_device, file)
        yield file, pass_cmd


@pytest.fixture(scope="session")
def _btrfs_device_persistent(_big_file_persistent):
    with NamedTemporaryFile() as ntf:
        btrfs_device = Path(ntf.name)
        shutil.copy(_big_file_persistent, btrfs_device)
        sdm.mkfs(btrfs_device, "btrfs")
        yield btrfs_device


@pytest.fixture
def btrfs_device(_btrfs_device_persistent):
    with NamedTemporaryFile() as ntf:
        file = Path(ntf.name)
        shutil.copy(_btrfs_device_persistent, file)
        yield file


@pytest.fixture(scope="session")
def _ext4_device_persistent(_big_file_persistent):
    with NamedTemporaryFile() as ntf:
        ext4_device = Path(ntf.name)
        shutil.copy(_big_file_persistent, ext4_device)
        sdm.mkfs_ext4(ext4_device)
        yield ext4_device


@pytest.fixture
def ext4_device(_ext4_device_persistent):
    with NamedTemporaryFile() as ntf:
        file = Path(ntf.name)
        shutil.copy(_ext4_device_persistent, file)
        yield file
