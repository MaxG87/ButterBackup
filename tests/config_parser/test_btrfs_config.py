from __future__ import annotations

import re
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
from hypothesis import assume, given
from hypothesis import strategies as st
from pydantic import ValidationError
from storage_device_managers import ValidCompressions

from butter_backup import config_parser as cp
from tests import hypothesis_utils as hu

TEST_RESOURCES = Path(__file__).parent.parent / "resources"
EXCLUDE_FILE = TEST_RESOURCES / "exclude-file"


@st.composite
def valid_unparsed_empty_btrfs_config(draw):
    config = draw(
        st.fixed_dictionaries(
            {
                "BackupRepositoryFolder": st.text(),
                "Compression": st.sampled_from(
                    [cur.value for cur in ValidCompressions]
                ),
                "ExcludePatternsFile": st.just(str(EXCLUDE_FILE)) | st.none(),
                "DevicePassCmd": st.text(),
                "Files": st.just([]),
                "FilesDest": st.text(),
                "Folders": st.just({}),
                "UUID": st.uuids().map(str),
            }
        )
    )
    return config


@given(base_config=valid_unparsed_empty_btrfs_config(), dest_dir=hu.filenames())
def test_btrfs_config_rejects_file_dest_collision(base_config, dest_dir: str):
    base_config["Folders"] = {
        "/usr/bin": "backup_bins",
        "/etc": dest_dir,
        "/var/log": "backup_logs",
    }
    base_config["FilesDest"] = dest_dir
    with NamedTemporaryFile() as src:
        base_config["Files"] = [src.name]
        with pytest.raises(ValidationError, match=re.escape(dest_dir)):
            cp.BtrFSRsyncConfig.model_validate(base_config)


@given(base_config=valid_unparsed_empty_btrfs_config(), file_name=hu.filenames())
def test_btrfs_config_rejects_filename_collision(base_config, file_name):
    base_config["Folders"] = {}
    with TemporaryDirectory() as td1:
        with TemporaryDirectory() as td2:
            dirs = [td1, td2]
            files = [Path(cur_dir) / file_name for cur_dir in dirs]
            for f in files:
                f.touch()
            base_config["Files"] = [str(f) for f in files]
            with pytest.raises(ValidationError, match=re.escape(file_name)):
                cp.BtrFSRsyncConfig.model_validate(base_config)


@given(base_config=valid_unparsed_empty_btrfs_config())
def test_btrfs_config_expands_user(base_config):
    with TemporaryDirectory() as dest:
        pass
    folders = {
        "/usr/bin": "backup_bins",
        "~": dest,
        "/var/log": "backup_logs",
    }
    base_config["Folders"] = folders
    with NamedTemporaryFile(dir=Path.home()) as src_file:
        fname = f"~/{Path(src_file.name).name}"
        base_config["Files"] = ["/bin/bash", fname]
        with NamedTemporaryFile(dir=Path.home()) as exclude_file:
            exclude_file_relative = f"~/{Path(exclude_file.name).name}"
            base_config["ExcludePatternsFile"] = exclude_file_relative
            cfg = cp.BtrFSRsyncConfig.model_validate(base_config)
    assert Path("~").expanduser() in cfg.Folders
    assert Path(src_file.name).expanduser() in cfg.Files
    assert cfg.ExcludePatternsFile == Path(exclude_file.name).expanduser()


@given(
    base_config=valid_unparsed_empty_btrfs_config(),
    folder_dest=hu.filenames(),
)
def test_btrfs_config_rejects_duplicate_dest(base_config, folder_dest: str):
    with TemporaryDirectory() as src1:
        with TemporaryDirectory() as src2:
            folders = {
                "/usr/bin": "backup_bins",
                src1: folder_dest,
                "/var/log": "backup_logs",
                src2: folder_dest,
            }
            base_config["Folders"] = folders
            base_config["Files"] = []
            with pytest.raises(ValidationError, match=re.escape(folder_dest)):
                cp.BtrFSRsyncConfig.model_validate(base_config)


@given(base_config=valid_unparsed_empty_btrfs_config())
def test_btrfs_config_uuid_is_mapname(base_config) -> None:
    cfg = cp.BtrFSRsyncConfig.model_validate(base_config)
    assert base_config["UUID"] == str(cfg.UUID)


@given(base_config=valid_unparsed_empty_btrfs_config())
def test_btrfs_config_device_ends_in_uuid(base_config) -> None:
    cfg = cp.BtrFSRsyncConfig.model_validate(base_config)
    uuid = base_config["UUID"]
    assert cfg.device() == Path(f"/dev/disk/by-uuid/{uuid}")


@given(
    base_config=valid_unparsed_empty_btrfs_config(),
    compression=st.sampled_from(["zsdd", "zlbi", "xkcd", "invalid-string", ""]),
)
def test_btrfs_config_rejects_invalid_compression(
    base_config, compression: str
) -> None:
    base_config["Compression"] = compression
    with pytest.raises(ValidationError):
        cp.BtrFSRsyncConfig.model_validate(base_config)


@given(
    base_config=valid_unparsed_empty_btrfs_config(),
    level=st.integers(max_value=0) | st.integers(min_value=16),
    algorithm=st.sampled_from(["lzo", "zstd", "zlib"]),
)
def test_btrfs_config_rejects_out_of_bounds_compression_level(
    base_config, level: int, algorithm: str
) -> None:
    base_config["Compression"] = f"{algorithm}:{level}"
    with pytest.raises(ValidationError):
        cp.BtrFSRsyncConfig.model_validate(base_config)


@given(
    base_config=valid_unparsed_empty_btrfs_config(),
    level=st.integers(min_value=1, max_value=9) | st.none(),
)
def test_btrfs_config_accepts_valid_zlib(base_config, level: int | None) -> None:
    compression = "zlib"
    if level is not None:
        compression += f":{level}"
    base_config["Compression"] = compression
    cfg = cp.BtrFSRsyncConfig.model_validate(base_config)
    assert cfg.Compression == ValidCompressions(compression)


@given(
    base_config=valid_unparsed_empty_btrfs_config(),
    level=st.integers(min_value=1, max_value=15) | st.none(),
)
def test_btrfs_config_accepts_valid_zstd(base_config, level: int | None) -> None:
    compression = "zstd"
    if level is not None:
        compression += f":{level}"
    base_config["Compression"] = compression
    cfg = cp.BtrFSRsyncConfig.model_validate(base_config)
    assert cfg.Compression == ValidCompressions(compression)


@given(
    base_config=valid_unparsed_empty_btrfs_config(),
)
def test_btrfs_config_accepts_valid_lzo(base_config) -> None:
    compression = "lzo"
    base_config["Compression"] = compression
    cfg = cp.BtrFSRsyncConfig.model_validate(base_config)
    assert cfg.Compression == ValidCompressions.LZO


@given(base_config=valid_unparsed_empty_btrfs_config(), folder_dest=hu.filenames())
def test_btrfs_config_json_roundtrip(base_config, folder_dest: str):
    assume(folder_dest != base_config["FilesDest"])
    with TemporaryDirectory() as src_folder:
        with NamedTemporaryFile() as src_file:
            base_config["Folders"] = {src_folder: folder_dest}
            base_config["Files"] = [src_file.name]
            cfg = cp.BtrFSRsyncConfig.model_validate(base_config)
            as_json = cfg.model_dump_json()
            deserialised = cp.BtrFSRsyncConfig.model_validate_json(as_json)
    assert cfg == deserialised
