from __future__ import annotations

import json
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from uuid import UUID

import pytest
from hypothesis import given
from hypothesis import strategies as st
from pydantic import ValidationError

from butter_backup import config_parser as cp

SUCCESS_CODES = {0, None}
NOF_FOLDER_BACKUP_MAPPING_ELEMS = 2


@st.composite
def valid_unparsed_configs(draw):
    config = draw(
        st.fixed_dictionaries(
            {
                "UUID": st.uuids().map(str),
                "PassCmd": st.text(),
                "Folders": st.lists(
                    st.lists(
                        st.text(min_size=1),
                        min_size=NOF_FOLDER_BACKUP_MAPPING_ELEMS,
                        max_size=NOF_FOLDER_BACKUP_MAPPING_ELEMS,
                    ),
                ),
                "Files": st.fixed_dictionaries(
                    {"destination": st.text(), "files": st.lists(st.text(min_size=1))}
                ),
            }
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
                "Folders": st.just([]),
                "UUID": st.uuids().map(str),
            }
        )
    )
    return config


def test_useful_error_on_missing_file_name() -> None:
    missing_cfg = Path("/path/to/nowhere/butter-backup.cfg")
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
    with TemporaryDirectory() as td:
        file_name = Path(td, "configuration")
        file_name.write_text("[]")
        with pytest.raises(SystemExit) as sysexit:
            list(cp.load_configuration(file_name))
        assert sysexit.value.code not in SUCCESS_CODES


@given(
    uuid=st.uuids(),
    pass_cmd=st.text(),
    backup_dest_dirs=st.lists(st.text(), min_size=2, max_size=2, unique=True),
)
def test_load_configuration_parses(
    uuid: UUID, pass_cmd: str, backup_dest_dirs: list[str]
) -> None:
    with TemporaryDirectory() as source:
        config = cp.ParsedButterConfig(
            uuid=uuid,
            pass_cmd=pass_cmd,
            folders={(source, backup_dest_dirs[0])},
            files_dest=backup_dest_dirs[1],
            files=set(),
        )
        butter_config = cp.ButterConfig.from_raw_config(config)
        with TemporaryDirectory() as td:
            file_name = Path(td, "configuration")
            file_name.write_text(json.dumps([config.as_dict()]))
            parse_result = list(cp.load_configuration(file_name))
        assert [butter_config] == parse_result


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
    incomplete_cfg=valid_unparsed_configs()
    .flatmap(
        lambda cfg_dict: st.lists(
            st.sampled_from(sorted(cfg_dict.items())),
            unique_by=lambda tup: tup[0],  # type: ignore
            max_size=len(cfg_dict) - 1,
        )
    )
    .map(dict)
)
def test_parsing_config_fails_on_missing_keys(incomplete_cfg) -> None:
    with pytest.raises(SystemExit) as sysexit:
        cp.ParsedButterConfig.from_dict(incomplete_cfg)
    assert sysexit.value.code not in SUCCESS_CODES


@given(
    config=valid_unparsed_configs(),
    invalid_folder_mapping=st.lists(st.text()).filter(
        lambda lst: len(lst) != NOF_FOLDER_BACKUP_MAPPING_ELEMS
    ),
)
def test_parsing_config_fails_on_malformed_folder_backup_mappings(
    config, invalid_folder_mapping
) -> None:
    config["Folders"].append(invalid_folder_mapping)
    with pytest.raises(SystemExit) as sysexit:
        cp.ParsedButterConfig.from_dict(config)
    assert sysexit.value.code not in SUCCESS_CODES


@given(config=valid_unparsed_configs())
def test_parsing_config_parses(config) -> None:
    cfg = cp.ParsedButterConfig.from_dict(config)
    assert str(cfg.uuid) == config["UUID"]
    assert cfg.pass_cmd == config["PassCmd"]
    assert cfg.files_dest == config["Files"]["destination"]
    assert cfg.folders == {tuple(elem) for elem in config["Folders"]}
    assert cfg.files == set(config["Files"]["files"])


@given(base_config=valid_unparsed_configs())
def test_butter_config_accepts_raw_config(base_config):
    with TemporaryDirectory() as src_folder:
        with TemporaryDirectory() as dest:
            with NamedTemporaryFile() as src_file:
                folders = [(src_folder, dest)]
                base_config["Folders"] = folders
                base_config["Files"]["files"] = [src_file.name]
                raw_config = cp.ParsedButterConfig.from_dict(base_config)
                cfg = cp.ButterConfig.from_raw_config(raw_config)
    assert cfg.pass_cmd == raw_config.pass_cmd
    assert str(cfg.device()).endswith(str(raw_config.uuid))
    assert {(str(src), str(dest)) for (src, dest) in cfg.folders} == raw_config.folders
    assert {str(file) for file in cfg.files} == raw_config.files
    assert cfg.files_dest == raw_config.files_dest
    assert cfg.uuid == raw_config.uuid


@given(date=st.dates(), uuid=st.uuids())
def test_butter_config_uuid_is_mapname(date, uuid) -> None:
    cfg = cp.ButterConfig(
        files=set(),
        files_dest="Einzeldateien",
        folders=set(),
        pass_cmd="echo supersecure",
        uuid=uuid,
    )
    assert str(cfg.uuid) == cfg.map_name()


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_missing_folder_src(base_config):
    folder_dest = "folder-destination"
    with TemporaryDirectory() as src:
        pass
    folders = [
        ("/usr/bin", "backup_bins"),
        (src, folder_dest),
        ("/var/log", "backup_logs"),
    ]
    base_config["Folders"] = folders
    base_config["Files"]["files"] = []
    raw_config = cp.ParsedButterConfig.from_dict(base_config)
    with pytest.raises(SystemExit) as sysexit:
        cp.ButterConfig.from_raw_config(raw_config)
    assert sysexit.value.code not in SUCCESS_CODES
    assert str(raw_config.uuid) in sysexit.value.code  # type: ignore
    assert src in sysexit.value.code  # type: ignore


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_non_dir_src(base_config):
    folder_dest = "folder-destination"
    with NamedTemporaryFile() as src:
        folders = [
            ("/usr/bin", "backup_bins"),
            (src.name, folder_dest),
            ("/var/log", "backup_logs"),
        ]
        base_config["Folders"] = folders
        base_config["Files"]["files"] = []
        raw_config = cp.ParsedButterConfig.from_dict(base_config)
        with pytest.raises(SystemExit) as sysexit:
            cp.ButterConfig.from_raw_config(raw_config)
    assert sysexit.value.code not in SUCCESS_CODES
    assert str(raw_config.uuid) in sysexit.value.code  # type: ignore
    assert src.name in sysexit.value.code  # type: ignore


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_missing_file_src(base_config):
    with NamedTemporaryFile() as src:
        pass
    base_config["Folders"] = []
    base_config["Files"]["files"] = [src.name]
    raw_config = cp.ParsedButterConfig.from_dict(base_config)
    with pytest.raises(SystemExit) as sysexit:
        cp.ButterConfig.from_raw_config(raw_config)
    assert sysexit.value.code not in SUCCESS_CODES
    assert str(raw_config.uuid) in sysexit.value.code  # type: ignore
    assert src.name in sysexit.value.code  # type: ignore


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_non_file_src(base_config):
    with TemporaryDirectory() as src:
        base_config["Files"]["files"] = [src]
        base_config["Folders"] = []
        raw_config = cp.ParsedButterConfig.from_dict(base_config)
        with pytest.raises(SystemExit) as sysexit:
            cp.ButterConfig.from_raw_config(raw_config)
    assert sysexit.value.code not in SUCCESS_CODES
    assert str(raw_config.uuid) in sysexit.value.code  # type: ignore
    assert src in sysexit.value.code  # type: ignore


@given(base_config=valid_unparsed_configs())
def test_butter_config_expands_user(base_config):
    with TemporaryDirectory() as dest:
        pass
    folders = [
        ("/usr/bin", "backup_bins"),
        ("~", dest),
        ("/var/log", "backup_logs"),
    ]
    base_config["Folders"] = folders
    with NamedTemporaryFile(dir=Path.home()) as src_file:
        fname = f"~/{Path(src_file.name).name}"
        base_config["Files"]["files"] = ["/bin/bash", fname]
        raw_config = cp.ParsedButterConfig.from_dict(base_config)
        cfg = cp.ButterConfig.from_raw_config(raw_config)
    assert Path("~").expanduser() in {src for (src, _) in cfg.folders}
    assert Path(src_file.name).expanduser() in cfg.files


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_duplicate_src(base_config):
    folder_dest = "folder-destination"
    with TemporaryDirectory() as src:
        folders = [
            ("/usr/bin", "backup_bins"),
            (src, f"{folder_dest}_1"),
            ("/var/log", "backup_logs"),
            (src, f"{folder_dest}_2"),
        ]
        base_config["Folders"] = folders
        base_config["Files"]["files"] = []
        raw_config = cp.ParsedButterConfig.from_dict(base_config)
        with pytest.raises(SystemExit) as sysexit:
            cp.ButterConfig.from_raw_config(raw_config)
        assert src in sysexit.value.code  # type: ignore


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_duplicate_dest(base_config):
    folder_dest = "folder-destination"
    with TemporaryDirectory() as src1:
        with TemporaryDirectory() as src2:
            folders = [
                ("/usr/bin", "backup_bins"),
                (src1, folder_dest),
                ("/var/log", "backup_logs"),
                (src2, folder_dest),
            ]
            base_config["Folders"] = folders
            base_config["Files"]["files"] = []
            raw_config = cp.ParsedButterConfig.from_dict(base_config)
            with pytest.raises(SystemExit) as sysexit:
                cp.ButterConfig.from_raw_config(raw_config)
            assert folder_dest in sysexit.value.code  # type: ignore


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_file_dest_collision(base_config):
    dest_dir = "destination-directory"
    base_config["Folders"] = [
        ("/usr/bin", "backup_bins"),
        ("/etc", dest_dir),
        ("/var/log", "backup_logs"),
    ]
    base_config["Files"]["destination"] = dest_dir
    with NamedTemporaryFile() as src:
        base_config["Files"]["files"] = [src.name]
        raw_config = cp.ParsedButterConfig.from_dict(base_config)
        with pytest.raises(SystemExit) as sysexit:
            cp.ButterConfig.from_raw_config(raw_config)
        assert dest_dir in sysexit.value.code  # type: ignore


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_filename_collision(base_config):
    file_name = "my-file-name"
    base_config["Folders"] = []
    with TemporaryDirectory() as td1:
        with TemporaryDirectory() as td2:
            dirs = [td1, td2]
            files = [Path(cur_dir) / file_name for cur_dir in dirs]
            for f in files:
                f.touch()
            base_config["Files"]["files"] = [str(f) for f in files]
            raw_config = cp.ParsedButterConfig.from_dict(base_config)
            with pytest.raises(SystemExit) as sysexit:
                cp.ButterConfig.from_raw_config(raw_config)
            assert file_name in sysexit.value.code  # type: ignore


@given(base_config=valid_unparsed_configs())
def test_butter_config_as_dict_roundtrip(base_config):
    parsed_1 = cp.ParsedButterConfig.from_dict(base_config)
    parsed_2 = cp.ParsedButterConfig.from_dict(parsed_1.as_dict())
    assert parsed_1 == parsed_2


@given(
    config=valid_unparsed_empty_btrfs_config(),
    invalid_folder_mapping=st.lists(st.text()).filter(
        lambda lst: len(lst) != NOF_FOLDER_BACKUP_MAPPING_ELEMS
    ),
)
def test_btrfs_config_rejects_malformed_folder_backup_mappings(
    config, invalid_folder_mapping
) -> None:
    config["Folders"].append(invalid_folder_mapping)
    with pytest.raises(ValidationError):
        cp.BtrfsConfig.parse_obj(config)


@given(base_config=valid_unparsed_empty_btrfs_config())
def test_btrfs_config_rejects_filename_collision(base_config):
    file_name = "my-file-name"
    base_config["Folders"] = []
    with TemporaryDirectory() as td1:
        with TemporaryDirectory() as td2:
            dirs = [td1, td2]
            files = [Path(cur_dir) / file_name for cur_dir in dirs]
            for f in files:
                f.touch()
            base_config["Files"] = [str(f) for f in files]
            with pytest.raises(ValidationError, match=file_name):
                cp.BtrfsConfig.parse_obj(base_config)
