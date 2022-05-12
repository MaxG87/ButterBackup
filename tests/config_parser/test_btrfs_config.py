from __future__ import annotations

import re
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from uuid import UUID

import pytest
from hypothesis import assume, given
from hypothesis import strategies as st
from pydantic import ValidationError

from butter_backup import config_parser as cp
from tests import hypothesis_utils as hu

NOF_FOLDER_BACKUP_MAPPING_ELEMS = 2


@st.composite
def valid_unparsed_configs(draw, may_be_incomplete: bool = False):
    item_strategy_mapping = {
        "UUID": st.uuids().map(str),
        "PassCmd": st.text(),
        "Folders": st.lists(
            st.lists(
                hu.filenames(),
                min_size=NOF_FOLDER_BACKUP_MAPPING_ELEMS,
                max_size=NOF_FOLDER_BACKUP_MAPPING_ELEMS,
            ),
        ),
        "Files": st.fixed_dictionaries(
            {
                "destination": st.text(),
                "files": st.lists(hu.filenames()),
            }
        ),
    }
    config = draw(
        st.fixed_dictionaries(
            {} if may_be_incomplete else item_strategy_mapping,
            optional=item_strategy_mapping if may_be_incomplete else {},
        )
    )
    return config


@st.composite
def valid_unparsed_empty_btrfs_config(draw):
    config = draw(
        st.fixed_dictionaries(
            {
                "PassCmd": st.text(),
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
            cp.BtrfsConfig.parse_obj(base_config)


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
                cp.BtrfsConfig.parse_obj(base_config)


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
        cfg = cp.BtrfsConfig.parse_obj(base_config)
    assert Path("~").expanduser() in cfg.Folders
    assert Path(src_file.name).expanduser() in cfg.Files


@given(
    base_config=valid_unparsed_configs(),
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
                cp.BtrfsConfig.parse_obj(base_config)


@given(files_dest=st.text(), pass_cmd=st.text(), uuid=st.uuids())
def test_btrfs_config_uuid_is_mapname(
    files_dest: str, pass_cmd: str, uuid: UUID
) -> None:
    cfg = cp.BtrfsConfig(
        Files=set(),
        FilesDest=files_dest,
        Folders={},
        PassCmd=pass_cmd,
        UUID=uuid,
    )
    assert str(uuid) == cfg.map_name()


@given(files_dest=st.text(), pass_cmd=st.text(), uuid=st.uuids())
def test_btrfs_config_device_ends_in_uuid(
    files_dest: str, pass_cmd: str, uuid: UUID
) -> None:
    cfg = cp.BtrfsConfig(
        Files=set(),
        FilesDest=files_dest,
        Folders={},
        PassCmd=pass_cmd,
        UUID=uuid,
    )
    assert cfg.device() == Path(f"/dev/disk/by-uuid/{cfg.UUID}")


@given(base_config=valid_unparsed_empty_btrfs_config(), folder_dest=hu.filenames())
def test_btrfs_config_json_roundtrip(base_config, folder_dest: str):
    assume(folder_dest != base_config["FilesDest"])
    with TemporaryDirectory() as src_folder:
        with NamedTemporaryFile() as src_file:
            base_config["Folders"] = {src_folder: folder_dest}
            base_config["Files"] = [src_file.name]
            cfg = cp.BtrfsConfig.parse_obj(base_config)
            as_json = cfg.json()
            deserialised = cp.BtrfsConfig.parse_raw(as_json)
    assert cfg == deserialised


@given(base_config=valid_unparsed_configs())
def test_btrfs_config_handles_old_style_config(base_config):
    with TemporaryDirectory() as src_folder:
        with TemporaryDirectory() as dest:
            with NamedTemporaryFile() as src_file:
                folders = [(src_folder, dest)]
                base_config["Folders"] = folders
                base_config["Files"]["files"] = [src_file.name]
                cfg = cp.BtrfsConfig.parse_obj(base_config)
    result_folders = {(str(src), dest) for src, dest in cfg.Folders.items()}
    assert cfg.PassCmd == base_config["PassCmd"]
    assert str(cfg.device()).endswith(base_config["UUID"])
    assert result_folders == set(base_config["Folders"])
    assert {str(file) for file in cfg.Files} == set(base_config["Files"]["files"])
    assert cfg.FilesDest == base_config["Files"]["destination"]
    assert str(cfg.UUID) == base_config["UUID"]
