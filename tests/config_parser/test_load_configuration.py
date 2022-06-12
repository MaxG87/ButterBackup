from __future__ import annotations

import json
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from uuid import UUID

import pytest
from hypothesis import given
from hypothesis import strategies as st

from butter_backup import config_parser as cp

SUCCESS_CODES = {0, None}


def test_useful_error_on_missing_file_name() -> None:
    with NamedTemporaryFile() as named_tmp:
        missing_cfg = Path(named_tmp.name)
    with pytest.raises(SystemExit) as sysexit:
        list(cp.load_configuration(missing_cfg))
    assert str(missing_cfg) in str(sysexit.value)
    assert "--help" in str(sysexit.value)


def test_load_configuration_rejects_missing_cfg() -> None:
    with NamedTemporaryFile() as named_tmp:
        file_name = Path(named_tmp.name)
    with pytest.raises(SystemExit) as sysexit:
        list(cp.load_configuration(file_name))
    assert sysexit.value.code not in SUCCESS_CODES


def test_load_configuration_rejects_empty_file() -> None:
    with NamedTemporaryFile() as named_tmp:
        config_file = Path(named_tmp.name)
        config_file.write_text("[]")
        with pytest.raises(SystemExit) as sysexit:
            list(cp.load_configuration(config_file))
        assert sysexit.value.code not in SUCCESS_CODES


@given(
    non_list=st.one_of(
        st.dictionaries(st.text(), st.text(), min_size=1), st.text(min_size=1)
    )
)
def test_load_configuration_warns_on_non_lists(non_list) -> None:
    with TemporaryDirectory() as td:
        file_name = Path(td, "configuration")
        file_name.write_text(json.dumps(non_list))
        with pytest.raises(SystemExit) as sysexit:
            list(cp.load_configuration(file_name))
    assert sysexit.value.code not in SUCCESS_CODES
    assert "muss eine JSON-Liste" in str(sysexit.value)


def test_load_configuration_warns_on_non_dict_item() -> None:
    with TemporaryDirectory() as td:
        file_name = Path(td, "configuration")
        file_name.write_text(json.dumps([{}, 1337]))
        with pytest.raises(SystemExit) as sysexit:
            list(cp.load_configuration(file_name))
    assert sysexit.value.code not in SUCCESS_CODES
    assert "Alle Einträge müssen" in str(sysexit.value)


@given(
    backup_dest_dirs=st.lists(st.text(), min_size=2, max_size=2, unique=True),
    backup_repository_folder=st.text(),
    pass_cmd=st.text(),
    uuid=st.uuids(),
)
def test_load_configuration_parses_btrfs_config(
    backup_dest_dirs: list[str],
    backup_repository_folder: str,
    pass_cmd: str,
    uuid: UUID,
) -> None:
    with TemporaryDirectory() as source:
        btrfs_cfg = cp.BtrfsConfig(
            BackupRepositoryFolder=backup_repository_folder,
            DevicePassCmd=pass_cmd,
            Files=set(),
            FilesDest=backup_dest_dirs[1],
            Folders={Path(source): backup_dest_dirs[0]},
            UUID=uuid,
        )
        with TemporaryDirectory() as td:
            file_name = Path(td, "configuration")
            file_name.write_text(f"[{btrfs_cfg.json()}]")
            parse_result = list(cp.load_configuration(file_name))
        assert [btrfs_cfg] == parse_result


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
        with TemporaryDirectory() as td:
            file_name = Path(td, "configuration")
            file_name.write_text(f"[{restic_cfg.json()}]")
            parse_result = list(cp.load_configuration(file_name))
        assert [restic_cfg] == parse_result
