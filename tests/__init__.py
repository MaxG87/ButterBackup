import typing as t
from pathlib import Path
from tempfile import NamedTemporaryFile

from butter_backup import config_parser as cp


def get_random_filename() -> str:
    with NamedTemporaryFile() as named_file:
        return named_file.name


@t.overload
def complement_configuration(
    config: cp.BtrFSRsyncConfig, source_dir: Path
) -> cp.BtrFSRsyncConfig: ...


@t.overload
def complement_configuration(
    config: cp.ResticConfig, source_dir: Path
) -> cp.ResticConfig: ...


def complement_configuration(
    config: cp.Configuration, source_dir: Path
) -> cp.Configuration:
    folders_root = source_dir / "backup-root"
    single_files = [
        source_dir / "config" / "docker" / "daemon.json",
        source_dir / "etc" / "fstab",
        source_dir / "cache" / "randomfile.bin",
    ]
    if isinstance(config, cp.BtrFSRsyncConfig):
        folder_dest_dir = "some-folder-name"
        return config.model_copy(
            update={
                "Folders": {folders_root: folder_dest_dir},
                "Files": single_files,
                "FilesDest": "Einzeldateien",
            }
        )
    if isinstance(config, cp.ResticConfig):
        return config.model_copy(
            update={"FilesAndFolders": {folders_root}.union(single_files)}
        )
    t.assert_never(config)
