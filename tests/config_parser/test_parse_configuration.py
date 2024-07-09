from __future__ import annotations

import json
from pathlib import Path
from tempfile import TemporaryDirectory
from uuid import UUID

import pytest
from hypothesis import given
from hypothesis import strategies as st
from pydantic import ValidationError

from butter_backup import config_parser as cp

SUCCESS_CODES = {0, None}


def test_parse_configuration_rejects_empty_list() -> None:
    with pytest.raises(SystemExit) as sysexit:
        cp.parse_configuration("[]")
    assert sysexit.value.code not in SUCCESS_CODES


@given(
    non_list=st.one_of(
        st.dictionaries(st.text(), st.text(), min_size=1), st.text(min_size=1)
    )
)
def test_parse_configuration_warns_on_non_lists(non_list) -> None:
    with pytest.raises(ValidationError):
        cp.parse_configuration(json.dumps(non_list))


def test_parse_configuration_warns_on_non_dict_item() -> None:
    with pytest.raises(ValidationError):
        cp.parse_configuration(json.dumps([{}, 1337]))


@given(
    backup_dest_dirs=st.lists(st.text(), min_size=2, max_size=2, unique=True),
    backup_repository_folder=st.text(),
    pass_cmd=st.text(),
    uuid=st.uuids(),
)
def test_parse_configuration_parses_btrfs_config(
    backup_dest_dirs: list[str],
    backup_repository_folder: str,
    pass_cmd: str,
    uuid: UUID,
) -> None:
    with TemporaryDirectory() as source:
        btrfs_cfg = cp.BtrFSRsyncConfig(
            BackupRepositoryFolder=backup_repository_folder,
            DevicePassCmd=pass_cmd,
            Files=set(),
            FilesDest=backup_dest_dirs[1],
            Folders={Path(source): backup_dest_dirs[0]},
            UUID=uuid,
        )
        cfg_lst = cp.parse_configuration(f"[{btrfs_cfg.model_dump_json()}]")
        assert cfg_lst == [btrfs_cfg]


@given(
    backup_dest_dirs=st.lists(st.text(), min_size=2, max_size=2, unique=True),
    backup_repository_folder=st.text(),
    device_pass_cmd=st.text(),
    repository_pass_cmd=st.text(),
    uuid=st.uuids(),
)
def test_load_configuration_parses_restic_config(
    backup_dest_dirs: list[str],
    backup_repository_folder: str,
    device_pass_cmd: str,
    repository_pass_cmd: str,
    uuid: UUID,
) -> None:
    with TemporaryDirectory() as source:
        restic_cfg = cp.ResticConfig(
            BackupRepositoryFolder=backup_repository_folder,
            DevicePassCmd=device_pass_cmd,
            FilesAndFolders={Path(source)},
            RepositoryPassCmd=repository_pass_cmd,
            UUID=uuid,
        )
        cfg_lst = cp.parse_configuration(f"[{restic_cfg.model_dump_json()}]")
        assert cfg_lst == [restic_cfg]
