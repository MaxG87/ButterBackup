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


def valid_toml_text(
    min_size: int = 1, max_size: int | None = None
) -> st.SearchStrategy[str]:
    """Generate strings that are valid in TOML basic strings.

    max_size should be set to 85 for valid path elements.
    """
    return st.text(
        alphabet=st.characters(
            blacklist_categories=["Cc", "Cs"],
            whitelist_characters="\t",
            max_codepoint=0xFFFF,
        ),
        min_size=min_size,
        max_size=max_size,
    )


def valid_path_components(min_size: int = 1) -> st.SearchStrategy[str]:
    """
    Generate valid path components

    The path components will also be vaild in TOML strings.
    """
    return valid_toml_text(min_size, max_size=85).filter(
        lambda n: "/" not in n and n not in {".", ".."}
    )


def valid_paths(min_depth=1) -> st.SearchStrategy[Path]:
    """
    Generate valid Unix paths

    The paths will also be vaild in TOML strings.
    """
    if min_depth < 1:
        raise ValueError("min_depth must be at least 1")
    min_component_size = 1
    return st.lists(valid_path_components(min_component_size), min_size=min_depth).map(
        lambda components: Path("/".join(components))
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
        DevicePassCmd=valid_toml_text(),
        Files=st.just([]),
        FilesDest=valid_path_components(),
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
        BackupRepositoryFolder=valid_path_components(),
        ExcludePatternsFile=st.just(exclude_file),
        DevicePassCmd=valid_toml_text(),
        FilesAndFolders=st.just([]),
        Name=st.none() | valid_path_components(),
        RepositoryPassCmd=valid_toml_text(),
        UUID=st.uuids(),
    )


def valid_unparsed_empty_restic_config(
    exclude_file: Path | None,
) -> st.SearchStrategy[dict[str, t.Any]]:
    return valid_empty_restic_config(exclude_file).map(
        lambda config: config.model_dump(mode="json")
    )
