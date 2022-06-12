from __future__ import annotations

from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from uuid import UUID

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
                "FilesAndFolders": st.just([]),
                "RepositoryPassCmd": st.text(),
                "UUID": st.uuids().map(str),
            }
        )
    )
    return config


@given(uuid=st.uuids(), device_passphrase=st.text(), repository_passphrase=st.text())
def test_restic_from_uuid_and_pashphrase(
    uuid: UUID, device_passphrase: str, repository_passphrase: str
) -> None:
    config = cp.ResticConfig.from_uuid_and_passphrases(
        uuid, device_passphrase, repository_passphrase
    )
    assert config.FilesAndFolders == set()
    assert config.UUID == uuid
    assert device_passphrase in config.DevicePassCmd
    assert repository_passphrase in config.RepositoryPassCmd


@pytest.mark.xfail(reason="safety checks not yet implemented")
@given(
    uuid=st.uuids(),
    passphrase=st.sampled_from(
        ["contains_'quote", "contains;_semicolon", "contains&ampersand"]
    ),
)
def test_restic_from_uuid_and_passphrase_rejects_unsafe_device_passphrase(
    uuid: UUID,
    passphrase: str,
) -> None:
    with pytest.raises(ValueError):
        cp.ResticConfig.from_uuid_and_passphrases(
            uuid, device_passphrase=passphrase, repository_passphrase="safe-passphrase"
        )


@pytest.mark.xfail(reason="safety checks not yet implemented")
@given(
    uuid=st.uuids(),
    passphrase=st.sampled_from(
        ["contains_'quote", "contains;_semicolon", "contains&ampersand"]
    ),
)
def test_restic_from_uuid_and_passphrase_rejects_unsafe_repository_passphrase(
    uuid: UUID,
    passphrase: str,
) -> None:
    with pytest.raises(ValueError):
        cp.ResticConfig.from_uuid_and_passphrases(
            uuid, device_passphrase="safe-passphrase", repository_passphrase=passphrase
        )


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
        cfg = cp.ResticConfig.parse_obj(base_config)
    expected = {Path("~").expanduser(), Path(src_file.name).expanduser()}
    assert cfg.FilesAndFolders == expected


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
            base_config["FilesAndFolders"] = {src_folder, src_file.name}
            cfg = cp.ResticConfig.parse_obj(base_config)
            as_json = cfg.json()
            deserialised = cp.ResticConfig.parse_raw(as_json)
    assert cfg == deserialised
