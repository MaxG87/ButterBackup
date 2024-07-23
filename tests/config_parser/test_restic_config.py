from __future__ import annotations

import json
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
import storage_device_managers as sdm
from hypothesis import given
from hypothesis import strategies as st
from pydantic import ValidationError

from butter_backup import config_parser as cp

TEST_RESOURCES = Path(__file__).parent.parent / "resources"
EXCLUDE_FILE = TEST_RESOURCES / "exclude-file"


def get_random_filename() -> str:
    with NamedTemporaryFile() as named_file:
        return named_file.name


@st.composite
def valid_unparsed_empty_restic_config(draw):
    config = draw(
        st.builds(
            cp.ResticConfig,
            BackupRepositoryFolder=st.text(),
            Compression=st.sampled_from([cur.value for cur in sdm.ValidCompressions]),
            ExcludePatternsFile=st.just(str(EXCLUDE_FILE)) | st.none(),
            DevicePassCmd=st.text(),
            FilesAndFolders=st.just([]),
            RepositoryPassCmd=st.text(),
            UUID=st.uuids(),
        )
    )
    return json.loads(config.model_dump_json())


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_rejects_missing_source(base_config):
    fname = get_random_filename()
    base_config["FilesAndFolders"] = {fname}
    with pytest.raises(ValidationError):
        cp.ResticConfig.model_validate(base_config)


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_expands_user(base_config):
    with NamedTemporaryFile(dir=Path.home()) as src_file:
        fname = f"~/{Path(src_file.name).name}"
        base_config["FilesAndFolders"] = {"~", fname}
        with NamedTemporaryFile(dir=Path.home()) as exclude_file:
            exclude_file_relative = f"~/{Path(exclude_file.name).name}"
            base_config["ExcludePatternsFile"] = exclude_file_relative
            cfg = cp.ResticConfig.model_validate(base_config)
    expected = {Path("~").expanduser(), Path(src_file.name).expanduser()}
    assert cfg.FilesAndFolders == expected
    assert cfg.ExcludePatternsFile == Path(exclude_file.name).expanduser()


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_uuid_is_mapname(base_config) -> None:
    cfg = cp.ResticConfig.model_validate(base_config)
    assert base_config["UUID"] == cfg.map_name()


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_device_ends_in_uuid(base_config) -> None:
    cfg = cp.ResticConfig.model_validate(base_config)
    uuid = base_config["UUID"]
    assert cfg.device() == Path(f"/dev/disk/by-uuid/{uuid}")


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_json_roundtrip(base_config):
    with TemporaryDirectory() as src_folder:
        with NamedTemporaryFile() as src_file:
            base_config["FilesAndFolders"] = {src_folder, src_file.name}
            cfg = cp.ResticConfig.model_validate(base_config)
            as_json = cfg.model_dump_json()
            deserialised = cp.ResticConfig.model_validate_json(as_json)
    assert cfg == deserialised
