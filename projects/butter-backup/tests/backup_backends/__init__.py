import typing as t
from collections import Counter, abc
from pathlib import Path
from tempfile import TemporaryDirectory

import shell_interface as sh

from butter_backup import backup_backends as bb
from butter_backup import config_parser as cp
from tests import complement_configuration


@t.overload
def get_expected_content(
    config: cp.BtrFSRsyncConfig, exclude_to_ignore_file: bool
) -> dict[Path, bytes]: ...
@t.overload
def get_expected_content(
    config: cp.ResticConfig, exclude_to_ignore_file: bool
) -> Counter[bytes]: ...
def get_expected_content(
    config: cp.DeviceConfiguration,
    exclude_to_ignore_file: bool,
) -> Counter[bytes] | dict[Path, bytes]:
    match config:
        case cp.BtrFSRsyncConfig():
            source_dirs = set(config.Folders)
            source_files = config.Files
        case cp.ResticConfig():
            source_dirs = {cur for cur in config.FilesAndFolders if cur.is_dir()}
            source_files = {cur for cur in config.FilesAndFolders if cur.is_file()}
        case _:
            t.assert_never(config)

    expected_content_dirs = get_expected_content_recursive_dir(
        source_dirs, exclude_to_ignore_file
    )
    expected_content_files = get_expected_content_single_files(source_files)
    match config:
        case cp.BtrFSRsyncConfig():
            expected_content_files_by_path = {
                Path(config.FilesDest) / key: value
                for key, value in expected_content_files.items()
            }
            return expected_content_dirs | expected_content_files_by_path
        case cp.ResticConfig():
            expected_content = expected_content_dirs | expected_content_files
            return Counter(expected_content.values())
        case _:
            t.assert_never(config)


def get_expected_content_recursive_dir(
    source_dirs: set[Path],
    exclude_to_ignore_file: bool,
) -> dict[Path, bytes]:
    expected_content = {}
    for cur_dir in source_dirs:
        cur_content = {
            file.relative_to(cur_dir): file.read_bytes()
            for file in list_files_recursively(cur_dir)
            if exclude_to_ignore_file is False or "ignore" not in file.name
        }
        expected_content.update(cur_content)
    return expected_content


def get_expected_content_single_files(source_files: set[Path]) -> dict[str, bytes]:
    expected_content = {file.name: file.read_bytes() for file in source_files}
    return expected_content


@t.overload
def get_result_content(
    config: cp.BtrFSRsyncConfig, mounted: Path
) -> dict[Path, bytes]: ...
@t.overload
def get_result_content(config: cp.ResticConfig, mounted: Path) -> Counter[bytes]: ...
def get_result_content(
    config: cp.DeviceConfiguration, mounted: Path
) -> Counter[bytes] | dict[Path, bytes]:
    match config:
        case cp.BtrFSRsyncConfig():
            return get_result_content_for_btrfs(config, mounted)
        case cp.ResticConfig():
            return get_result_content_for_restic(config, mounted)
        case _:
            t.assert_never(config)


def get_result_content_for_btrfs(
    config: cp.BtrFSRsyncConfig, mounted: Path
) -> dict[Path, bytes]:
    folder_dest_by_config = next(iter(config.Folders.values()))
    backup_repository = mounted / config.BackupRepositoryFolder
    latest_snapshot = sorted(backup_repository.iterdir())[-1]

    folder_dest_dir = latest_snapshot / folder_dest_by_config
    folder_content = {
        file.relative_to(folder_dest_dir): file.read_bytes()
        for file in list_files_recursively(folder_dest_dir)
    }
    files_dest = latest_snapshot / config.FilesDest
    files_content = {
        file.relative_to(latest_snapshot): file.read_bytes()
        for file in list_files_recursively(files_dest)
    }
    return folder_content | files_content


def get_result_content_for_restic(
    config: cp.ResticConfig, mounted: Path
) -> Counter[bytes]:
    with TemporaryDirectory() as restore_dir:
        restore_cmd: sh.StrPathList = [
            "restic",
            "-r",
            mounted / config.BackupRepositoryFolder,
            "restore",
            "latest",
            "--target",
            restore_dir,
        ]
        sh.pipe_pass_cmd_to_real_cmd(config.RepositoryPassCmd, restore_cmd)
        return Counter(
            file.read_bytes() for file in list_files_recursively(Path(restore_dir))
        )


def list_files_recursively(path: Path) -> abc.Iterable[Path]:
    for file_or_folder in path.rglob("*"):
        if file_or_folder.is_file():
            yield file_or_folder


def run_backup_cycle(
    base_config: cp.DeviceConfiguration,
    source_dir: Path,
    device: Path,
    config_extension: dict[str, t.Any] | None = None,
) -> cp.DeviceConfiguration:
    config = complement_configuration(base_config, source_dir)
    if config_extension is not None:
        config = config.model_copy(update=config_extension)
    backend = bb.BackupBackend.from_config(config)
    backend.do_backup(device)
    return config
