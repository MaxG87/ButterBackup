from __future__ import annotations

import json
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
from hypothesis import given
from hypothesis import strategies as st

from butter_backup import __main__ as bb

SUCCESS_CODES = {0, None}
NOF_FOLDER_BACKUP_MAPPING_ELEMS = 2

valid_uuids = st.text(
    st.characters(whitelist_categories=["Nd", "Lu", "Ll"]), min_size=1
)
valid_unparsed_configs = st.builds(
    dict,
    UUID=valid_uuids,
    PassCmd=st.text(),
    Folders=st.lists(
        st.lists(
            st.text(min_size=1),
            min_size=NOF_FOLDER_BACKUP_MAPPING_ELEMS,
            max_size=NOF_FOLDER_BACKUP_MAPPING_ELEMS,
        )
    ),
)


def test_load_configuration_rejects_missing_cfg() -> None:
    with NamedTemporaryFile() as named_tmp:
        file_name = Path(named_tmp.name)
    with pytest.raises(SystemExit) as sysexit:
        bb.load_configuration(file_name)
    assert sysexit.value.code not in SUCCESS_CODES


def test_load_configuration_rejects_empty_file() -> None:
    with TemporaryDirectory() as td:
        file_name = Path(td, "configuration")
        file_name.write_text("[]")
        with pytest.raises(SystemExit) as sysexit:
            bb.load_configuration(file_name)
        assert sysexit.value.code not in SUCCESS_CODES


@given(config=valid_unparsed_configs)
def test_load_configuration_parses(config) -> None:
    config["Folders"] = [list(route) for route in config["Folders"]]
    with TemporaryDirectory() as td:
        file_name = Path(td, "configuration")
        file_name.write_text(json.dumps(config))
        reread_config = bb.load_configuration(file_name)
        assert reread_config == config


@given(
    incomplete_cfg=valid_unparsed_configs.flatmap(
        lambda cfg_dict: st.lists(
            st.sampled_from(sorted(cfg_dict.items())),
            unique_by=lambda tup: tup[0],  # type: ignore
            max_size=len(cfg_dict) - 1,
        )
    ).map(dict)
)
def test_parsing_config_fails_on_missing_keys(incomplete_cfg) -> None:
    with pytest.raises(SystemExit) as sysexit:
        bb.ParsedButterConfig.from_dict(incomplete_cfg)
    assert sysexit.value.code not in SUCCESS_CODES


@given(
    config=valid_unparsed_configs,
    invalid_folder_mapping=st.lists(st.text()).filter(
        lambda lst: len(lst) != NOF_FOLDER_BACKUP_MAPPING_ELEMS
    ),
)
def test_parsing_config_fails_on_malformed_folder_backiup_mappings(
    config, invalid_folder_mapping
) -> None:
    config["Folders"].append(invalid_folder_mapping)
    with pytest.raises(SystemExit) as sysexit:
        bb.ParsedButterConfig.from_dict(config)
    assert sysexit.value.code not in SUCCESS_CODES


@given(config=valid_unparsed_configs)
def test_parsing_config_parses(config) -> None:
    cfg = bb.ParsedButterConfig.from_dict(config)
    assert cfg.uuid == config["UUID"]
    assert cfg.pass_cmd == config["PassCmd"]
    for src, dest in cfg.folders:
        assert [src, dest] in config["Folders"]


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_accepts_raw_config(uuid: str, pass_cmd: str):
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as dest:
            raw_config = bb.ParsedButterConfig(
                uuid=uuid, pass_cmd=pass_cmd, folders=[(src, dest)]
            )
            cfg = bb.ButterConfig.from_raw_config(raw_config)
    assert cfg.pass_cmd == raw_config.pass_cmd
    assert str(cfg.device).endswith(raw_config.uuid)
    assert [(str(cfg.folders[0][0]), str(cfg.folders[0][1]))] == raw_config.folders


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_rejects_missing_src(uuid: str, pass_cmd: str):
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as dest:
            pass
    folders = [
        ("/usr/bin", "backup_bins"),
        (src, dest),
        ("/var/log", "backup_logs"),
    ]
    raw_config = bb.ParsedButterConfig(uuid=uuid, pass_cmd=pass_cmd, folders=folders)
    with pytest.raises(SystemExit) as sysexit:
        bb.ButterConfig.from_raw_config(raw_config)
    assert sysexit.value.code not in SUCCESS_CODES
    assert uuid in sysexit.value.code  # type: ignore
    assert src in sysexit.value.code  # type: ignore


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_rejects_non_dir_src(uuid: str, pass_cmd: str):
    with NamedTemporaryFile() as src:
        with TemporaryDirectory() as dest:
            pass
        folders = [
            ("/usr/bin", "backup_bins"),
            (src.name, dest),
            ("/var/log", "backup_logs"),
        ]
        raw_config = bb.ParsedButterConfig(
            uuid=uuid, pass_cmd=pass_cmd, folders=folders
        )
        with pytest.raises(SystemExit) as sysexit:
            bb.ButterConfig.from_raw_config(raw_config)
    assert sysexit.value.code not in SUCCESS_CODES
    assert uuid in sysexit.value.code  # type: ignore
    assert src.name in sysexit.value.code  # type: ignore


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_expands_user(uuid: str, pass_cmd: str):
    with TemporaryDirectory() as dest:
        pass
    folders = [
        ("/usr/bin", "backup_bins"),
        ("~", dest),
        ("/var/log", "backup_logs"),
    ]
    raw_config = bb.ParsedButterConfig(uuid=uuid, pass_cmd=pass_cmd, folders=folders)
    cfg = bb.ButterConfig.from_raw_config(raw_config)
    assert Path("~").expanduser() in {src for (src, _) in cfg.folders}


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_rejects_duplicate_src(uuid: str, pass_cmd: str):
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as dest1:
            with TemporaryDirectory() as dest2:
                pass
        folders = [
            ("/usr/bin", "backup_bins"),
            (src, dest1),
            ("/var/log", "backup_logs"),
            (src, dest2),
        ]
        raw_config = bb.ParsedButterConfig(
            uuid=uuid, pass_cmd=pass_cmd, folders=folders
        )
        with pytest.raises(SystemExit) as sysexit:
            bb.ButterConfig.from_raw_config(raw_config)
        assert src in sysexit.value.code  # type: ignore


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_rejects_duplicate_dest(uuid: str, pass_cmd: str):
    with TemporaryDirectory() as src1:
        with TemporaryDirectory() as src2:
            with TemporaryDirectory() as dest:
                pass
            folders = [
                ("/usr/bin", "backup_bins"),
                (src1, dest),
                ("/var/log", "backup_logs"),
                (src2, dest),
            ]
            raw_config = bb.ParsedButterConfig(
                uuid=uuid, pass_cmd=pass_cmd, folders=folders
            )
            with pytest.raises(SystemExit) as sysexit:
                bb.ButterConfig.from_raw_config(raw_config)
            assert dest in sysexit.value.code  # type: ignore
