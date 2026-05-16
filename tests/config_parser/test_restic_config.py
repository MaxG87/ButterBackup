from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
from hypothesis import given
from hypothesis import strategies as st
from pydantic import ValidationError

from butter_backup import config_parser as cp
from tests import get_random_filename
from tests import hypothesis_utils as hu

TEST_RESOURCES = Path(__file__).parent.parent / "resources"
EXCLUDE_FILE = TEST_RESOURCES / "exclude-file"

_RawResticConfigT = dict[str, str | Path | None]


def valid_unparsed_empty_restic_config() -> st.SearchStrategy[_RawResticConfigT]:
    return hu.valid_unparsed_empty_restic_config(EXCLUDE_FILE)


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
def test_restic_config_name_defaults_to_uuid(base_config) -> None:
    base_config.pop("Name", None)
    cfg = cp.ResticConfig.model_validate(base_config)
    assert cfg.Name == base_config["UUID"]


@given(
    base_config=valid_unparsed_empty_restic_config(),
    custom_name=hu.valid_path_components(),
)
def test_restic_config_accepts_custom_name(base_config, custom_name: str) -> None:
    base_config["Name"] = custom_name
    cfg = cp.ResticConfig.model_validate(base_config)
    assert cfg.Name == custom_name


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_uuid_is_mapname(base_config) -> None:
    cfg = cp.ResticConfig.model_validate(base_config)
    assert base_config["UUID"] == str(cfg.UUID)


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


@pytest.mark.parametrize(
    "invalid_name",
    [
        "foo/bar",
        "/absolute",
        ".",
        "..",
        "null\x00byte",
        "",
    ],
)
def test_restic_config_rejects_invalid_name(invalid_name: str) -> None:
    with TemporaryDirectory() as source:
        config = {
            "BackupRepositoryFolder": "repo",
            "DevicePassCmd": "echo pass",
            "FilesAndFolders": [source],
            "Name": invalid_name,
            "RepositoryPassCmd": "echo repo-pass",
            "UUID": "12345678-1234-5678-1234-567812345678",
        }
        with pytest.raises(ValidationError):
            cp.ResticConfig.model_validate(config)


@given(base_config=valid_unparsed_empty_restic_config())
def test_restic_config_rejects_missing_exclude_patterns_file(
    base_config: _RawResticConfigT,
) -> None:
    missing_file = TEST_RESOURCES / "missing-exclude-file"
    errmsg_pattern = f"Path does not point to a file .*{missing_file.name}"
    base_config["ExcludePatternsFile"] = missing_file
    with pytest.raises(ValidationError, match=errmsg_pattern):
        cp.ResticConfig.model_validate(base_config)
