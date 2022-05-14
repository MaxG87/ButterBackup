from __future__ import annotations

import re
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
from hypothesis import given
from hypothesis import strategies as st
from pydantic import ValidationError

from butter_backup import config_parser as cp
from tests import hypothesis_utils as hu


@st.composite
def valid_unparsed_empty_restic_config(draw):
    config = draw(
        st.fixed_dictionaries(
            {
                "DevicePassCmd": st.text(),
                "Files": st.just([]),
                "Folders": st.just({}),
                "RepositoryPassCmd": st.text(),
                "UUID": st.uuids().map(str),
            }
        )
    )
    return config


@given(base_config=valid_unparsed_empty_restic_config(), file_name=hu.filenames())
def test_restic_config_rejects_filename_collision(base_config, file_name):
    base_config["Folders"] = {}
    with TemporaryDirectory() as td1:
        with TemporaryDirectory() as td2:
            dirs = [td1, td2]
            files = [Path(cur_dir) / file_name for cur_dir in dirs]
            for f in files:
                f.touch()
            base_config["Files"] = [str(f) for f in files]
            with pytest.raises(ValidationError, match=re.escape(file_name)):
                cp.ResticConfig.parse_obj(base_config)


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_expands_user(base_config):
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
        cfg = cp.ResticConfig.parse_obj(base_config)
    assert Path("~").expanduser() in cfg.Folders
    assert Path(src_file.name).expanduser() in cfg.Files


@given(
    base_config=valid_unparsed_empty_restic_config(),
    folder_dest=hu.filenames(),
)
def test_restic_config_rejects_duplicate_dest(base_config, folder_dest: str):
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
                cp.ResticConfig.parse_obj(base_config)


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_uuid_is_mapname(base_config) -> None:
    cfg = cp.ResticConfig.parse_obj(base_config)
    assert base_config["UUID"] == cfg.map_name()


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_device_ends_in_uuid(base_config) -> None:
    cfg = cp.ResticConfig.parse_obj(base_config)
    uuid = base_config["UUID"]
    assert cfg.device() == Path(f"/dev/disk/by-uuid/{uuid}")


@given(base_config=valid_unparsed_empty_restic_config(), folder_dest=hu.filenames())
def test_restic_config_json_roundtrip(base_config, folder_dest: str):
    with TemporaryDirectory() as src_folder:
        with NamedTemporaryFile() as src_file:
            base_config["Folders"] = {src_folder: folder_dest}
            base_config["Files"] = [src_file.name]
            cfg = cp.ResticConfig.parse_obj(base_config)
            as_json = cfg.json()
            deserialised = cp.ResticConfig.parse_raw(as_json)
    assert cfg == deserialised
