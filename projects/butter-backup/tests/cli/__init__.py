import typing as t
from pathlib import Path

from butter_backup import config_parser as cp


def in_docker_container() -> bool:
    return Path("/.dockerenv").exists()


def prepare_tmp_path(config: cp.DeviceConfiguration, parent: Path) -> None:
    if isinstance(config, cp.BtrFSRsyncConfig):
        _prepare_tmp_path_for_btrfs(config, parent)
    elif isinstance(config, cp.ResticConfig):
        _prepare_tmp_path_for_restic(config)
    else:
        t.assert_never(config)


def _prepare_tmp_path_for_btrfs(config: cp.BtrFSRsyncConfig, parent: Path) -> None:
    for cur in config.Folders:
        cur.mkdir(exist_ok=True)
    for cur in config.Files:
        cur.parent.mkdir(exist_ok=True, parents=True)
        cur.touch()
    (parent / config.FilesDest).mkdir(exist_ok=True)


def _prepare_tmp_path_for_restic(config: cp.ResticConfig) -> None:
    for cur in config.FilesAndFolders:
        # For the purpose of the test that uses this helper function,
        # test_do_backup_refuses_backup_when_device_is_already_open, it does not matter
        # what the content of tmp_path is as long as the configuration can be parsed.
        # Therefore, all items will be folders, since this is much easier to achieve.
        cur.mkdir(parents=True, exist_ok=True)
