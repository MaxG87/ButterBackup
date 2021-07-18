from __future__ import annotations

from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest

from butter_backup import shell_interface as sh

_PassPhrase = "supersecure"


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

    min_size = 128 * 1024 ** 2  # ~109MiB is the minimum size for BtrFS
    with TemporaryDirectory() as tempdir:
        filename = get_random_filename(dir_=tempdir)
        file = Path(filename)
        with file.open("wb") as fh:
            fh.write(bytes(min_size))
        yield file


@pytest.fixture
def btrfs_device(big_file: Path):
    cmd: sh.StrPathList = ["sudo", "mkfs.btrfs", big_file]
    sh.run_cmd(cmd=cmd)
    yield big_file


@pytest.fixture
def decrypted_device(big_file: Path):
    password_cmd: sh.StrPathList = ["echo", _PassPhrase]
    format_cmd: sh.StrPathList = ["sudo", "cryptsetup", "luksFormat", big_file]
    sh.run_piped_commands(cmds=[password_cmd, format_cmd])
    yield _PassPhrase, big_file
