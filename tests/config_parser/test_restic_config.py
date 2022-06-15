from __future__ import annotations

from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
from hypothesis import given
from hypothesis import strategies as st
from pydantic import ValidationError

from butter_backup import config_parser as cp

TEST_RESOURCES = Path(__file__).parent.parent / "resources"
EXCLUDE_FILE = TEST_RESOURCES / "exclude-file"


@st.composite
def valid_unparsed_empty_restic_config(draw):
    config = draw(
        st.fixed_dictionaries(
            {
                "BackupRepositoryFolder": st.text(),
                "ExcludePatternsFile": st.just(str(EXCLUDE_FILE)) | st.none(),
                "DevicePassCmd": st.text(),
                "FilesAndFolders": st.just([]),
                "RepositoryPassCmd": st.text(),
                "UUID": st.uuids().map(str),
            }
        )
    )
    return config


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_rejects_missing_source(base_config):
    with NamedTemporaryFile() as src_file:
        fname = src_file.name
    base_config["FilesAndFolders"] = {fname}
    with pytest.raises(ValidationError):
        cp.ResticConfig.parse_obj(base_config)


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_expands_user(base_config):
    with NamedTemporaryFile(dir=Path.home()) as src_file:
        fname = f"~/{Path(src_file.name).name}"
        base_config["FilesAndFolders"] = {"~", fname}
        with NamedTemporaryFile(dir=Path.home()) as exclude_file:
            exclude_file_relative = f"~/{Path(exclude_file.name).name}"
            base_config["ExcludePatternsFile"] = exclude_file_relative
            cfg = cp.ResticConfig.parse_obj(base_config)
    expected = {Path("~").expanduser(), Path(src_file.name).expanduser()}
    assert cfg.FilesAndFolders == expected
    assert cfg.ExcludePatternsFile == Path(exclude_file.name).expanduser()


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_uuid_is_mapname(base_config) -> None:
    cfg = cp.ResticConfig.parse_obj(base_config)
    assert base_config["UUID"] == cfg.map_name()


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_device_ends_in_uuid(base_config) -> None:
    cfg = cp.ResticConfig.parse_obj(base_config)
    uuid = base_config["UUID"]
    assert cfg.device() == Path(f"/dev/disk/by-uuid/{uuid}")


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_json_roundtrip(base_config):
    with TemporaryDirectory() as src_folder:
        with NamedTemporaryFile() as src_file:
            base_config["FilesAndFolders"] = {src_folder, src_file.name}
            cfg = cp.ResticConfig.parse_obj(base_config)
            as_json = cfg.json()
            deserialised = cp.ResticConfig.parse_raw(as_json)
    assert cfg == deserialised
