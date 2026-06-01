import typing as t
from pathlib import Path

from hypothesis import strategies as st
from storage_device_managers import ValidCompressions

from butter_backup import config_parser as cp


def filenames(min_size=1) -> st.SearchStrategy[str]:
    alpha = "abcdefghijklmnopqrstuvwxyzäöu"
    num = "01234567890"
    special = "_-.,() "
    permitted_chars = f"{alpha}{alpha.upper()}{num}{special}"
    return st.text(permitted_chars, min_size=min_size).filter(
        lambda fname: fname not in {".", ".."}
    )


def valid_path_components(min_size=1) -> st.SearchStrategy[str]:
    return st.text(min_size=min_size, max_size=128).filter(
        lambda n: "/" not in n and "\x00" not in n and n not in {".", ".."}
    )


def valid_unparsed_empty_btrfs_config(
    exclude_file: Path | None,
) -> st.SearchStrategy[dict[str, t.Any]]:
    return valid_empty_btrfs_config(exclude_file).map(
        lambda config: config.model_dump(mode="json")
    )


def valid_empty_btrfs_config(
    exclude_file: Path | None,
) -> st.SearchStrategy[cp.BtrFSRsyncConfig]:
    return st.builds(
        cp.BtrFSRsyncConfig,
        Name=st.none() | valid_path_components(),
        BackupRepositoryFolder=valid_path_components(),
        Compression=valid_compressions(),
        ExcludePatternsFile=st.just(exclude_file),
        DevicePassCmd=st.text(),
        Files=st.just([]),
        FilesDest=st.text(),
        Folders=st.just({}),
        UUID=st.uuids().map(str),
    )


def valid_compressions() -> st.SearchStrategy[ValidCompressions]:
    return st.sampled_from(list(ValidCompressions))


def valid_empty_restic_config(
    exclude_file: Path | None,
) -> st.SearchStrategy[cp.ResticConfig]:
    return st.builds(
        cp.ResticConfig,
        BackupRepositoryFolder=st.text(),
        ExcludePatternsFile=st.just(exclude_file),
        DevicePassCmd=st.text(),
        FilesAndFolders=st.just([]),
        Name=st.none() | valid_path_components(),
        RepositoryPassCmd=st.text(),
        UUID=st.uuids(),
    )


def valid_unparsed_empty_restic_config(
    exclude_file: Path | None,
) -> st.SearchStrategy[dict[str, t.Any]]:
    return valid_empty_restic_config(exclude_file).map(
        lambda config: config.model_dump(mode="json")
    )
