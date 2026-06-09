from __future__ import annotations

import shutil
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


@pytest.fixture(scope="session")
def _encrypted_btrfs_device_persistent(_big_file_persistent):
    password_cmd = sdm.generate_passcmd()
    with NamedTemporaryFile() as ntf:
        btrfs_device = Path(ntf.name)
        shutil.copy(_big_file_persistent, btrfs_device)
        sdm.encrypt_device(btrfs_device, password_cmd)
        with sdm.decrypted_device(btrfs_device, password_cmd) as decrypted:
            sdm.mkfs_btrfs(decrypted)
        yield btrfs_device, password_cmd


@pytest.fixture
def encrypted_btrfs_device(_encrypted_btrfs_device_persistent):
    """
    Create encrypted BtrFS file system

    Returns
    -------
    Path
        destination of encrypted BtrFS file system
    str
        password command that echos password to STDOUT
    """
    btrfs_device, pass_cmd = _encrypted_btrfs_device_persistent
    with NamedTemporaryFile() as ntf:
        file = Path(ntf.name)
        shutil.copy(btrfs_device, file)
        yield file, pass_cmd


@pytest.fixture(params=["encrypted_btrfs_device"])
def encrypted_device(request):
    # As of now, the fixture seems superfluous. However, it is kept in case
    # support for other file systems is added later on.
    dest, pass_cmd = request.getfixturevalue(request.param)
    return dest, pass_cmd


@pytest.fixture(scope="session")
def _btrfs_device_persistent(_big_file_persistent):
    with NamedTemporaryFile() as ntf:
        btrfs_device = Path(ntf.name)
        shutil.copy(_big_file_persistent, btrfs_device)
        sdm.mkfs_btrfs(btrfs_device)
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
