from __future__ import annotations

import json
import re
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
def filenames(draw, min_size=1) -> str:
    alpha = "abcdefghijklmnopqrstuvwxyzäöu"
    num = "01234567890"
    special = "_-.,() "
    permitted_chars = f"{alpha}{alpha.upper()}{num}{special}"
    fname: str = draw(
        st.text(permitted_chars, min_size=min_size).filter(
            lambda fname: fname not in {".", ".."}
        )
    )
    return fname


@st.composite
def valid_unparsed_configs(draw, may_be_incomplete: bool = False):
    item_strategy_mapping = {
        "UUID": st.uuids().map(str),
        "PassCmd": st.text(),
        "Folders": st.lists(
            st.lists(
                filenames(),
                min_size=NOF_FOLDER_BACKUP_MAPPING_ELEMS,
                max_size=NOF_FOLDER_BACKUP_MAPPING_ELEMS,
            ),
        ),
        "Files": st.fixed_dictionaries(
            {
                "destination": st.text(),
                "files": st.lists(filenames()),
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
        btrfs_cfg = cp.BtrfsConfig.parse_obj(
            {
                "UUID": uuid,
                "PassCmd": pass_cmd,
                "Folders": [[source, backup_dest_dirs[0]]],
                "FilesDest": backup_dest_dirs[1],
                "Files": [],
            }
        )
        butter_config = cp.ButterConfig(
            files=btrfs_cfg.Files,
            files_dest=btrfs_cfg.FilesDest,
            folders=btrfs_cfg.Folders,
            pass_cmd=btrfs_cfg.PassCmd,
            uuid=btrfs_cfg.UUID,
        )
        with TemporaryDirectory() as td:
            file_name = Path(td, "configuration")
            file_name.write_text(f"[{btrfs_cfg.json()}]")
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
    incomplete_cfg=valid_unparsed_configs(may_be_incomplete=True).filter(
        lambda cfg: len(cfg) < 4
    )
)
def test_parsing_config_fails_on_missing_keys(incomplete_cfg) -> None:
    with pytest.raises(ValidationError) as exc:
        cp.BtrfsConfig.parse_obj(incomplete_cfg)
    assert any(cur["msg"] == "field required" for cur in exc.value.errors())


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
    with pytest.raises(ValidationError) as valerr:
        cp.BtrfsConfig.parse_obj(config)

    errmsg_regex = re.compile("(too many|not enough) values to unpack")
    assert any(errmsg_regex.match(cur["msg"]) for cur in valerr.value.errors())


@given(base_config=valid_unparsed_configs())
def test_butter_config_accepts_raw_config(base_config):
    with TemporaryDirectory() as src_folder:
        with TemporaryDirectory() as dest:
            with NamedTemporaryFile() as src_file:
                folders = [(src_folder, dest)]
                base_config["Folders"] = folders
                base_config["Files"]["files"] = [src_file.name]
                cfg = cp.BtrfsConfig.parse_obj(base_config)
    assert cfg.PassCmd == base_config["PassCmd"]
    assert str(cfg.device()).endswith(base_config["UUID"])
    assert {(str(src), str(dest)) for (src, dest) in cfg.Folders} == set(
        base_config["Folders"]
    )
    assert {str(file) for file in cfg.Files} == set(base_config["Files"]["files"])
    assert cfg.FilesDest == base_config["Files"]["destination"]
    assert str(cfg.UUID) == base_config["UUID"]


@given(files_dest=st.text(), pass_cmd=st.text(), uuid=st.uuids())
def test_butter_config_uuid_is_mapname(
    files_dest: str, pass_cmd: str, uuid: UUID
) -> None:
    cfg = cp.BtrfsConfig(
        Files=set(),
        FilesDest=files_dest,
        Folders=set(),
        PassCmd=pass_cmd,
        UUID=uuid,
    )
    assert str(uuid) == cfg.map_name()


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
    with pytest.raises(ValidationError, match=re.escape(src)):
        cp.BtrfsConfig.parse_obj(base_config)


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
        with pytest.raises(ValueError, match=re.escape(src.name)):
            cp.BtrfsConfig.parse_obj(base_config)


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_missing_file_src(base_config):
    with NamedTemporaryFile() as src:
        pass
    base_config["Folders"] = []
    base_config["Files"]["files"] = [src.name]
    with pytest.raises(ValueError, match=re.escape(src.name)):
        cp.BtrfsConfig.parse_obj(base_config)


@given(base_config=valid_unparsed_configs())
def test_butter_config_rejects_non_file_src(base_config):
    with TemporaryDirectory() as src:
        base_config["Files"]["files"] = [src]
        base_config["Folders"] = []
        with pytest.raises(ValueError, match=re.escape(src)):
            cp.BtrfsConfig.parse_obj(base_config)


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
        cfg = cp.BtrfsConfig.parse_obj(base_config)
    assert Path("~").expanduser() in {src for (src, _) in cfg.Folders}
    assert Path(src_file.name).expanduser() in cfg.Files


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
        with pytest.raises(ValidationError, match=re.escape(src)):
            cp.BtrfsConfig.parse_obj(base_config)


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
            with pytest.raises(ValidationError, match=re.escape(folder_dest)):
                cp.BtrfsConfig.parse_obj(base_config)


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
        with pytest.raises(ValidationError, match=re.escape(dest_dir)):
            cp.BtrfsConfig.parse_obj(base_config)


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
            with pytest.raises(ValidationError, match=re.escape(file_name)):
                cp.BtrfsConfig.parse_obj(base_config)


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


@given(base_config=valid_unparsed_empty_btrfs_config(), dest_dir=filenames())
def test_btrfs_config_rejects_file_dest_collision(base_config, dest_dir: str):
    base_config["Folders"] = [
        ["/usr/bin", "backup_bins"],
        ["/etc", dest_dir],
        ["/var/log", "backup_logs"],
    ]
    base_config["FilesDest"] = dest_dir
    with NamedTemporaryFile() as src:
        base_config["Files"] = [src.name]
        with pytest.raises(ValidationError, match=re.escape(dest_dir)):
            cp.BtrfsConfig.parse_obj(base_config)


@given(base_config=valid_unparsed_empty_btrfs_config(), file_name=filenames())
def test_btrfs_config_rejects_filename_collision(base_config, file_name):
    base_config["Folders"] = []
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
    folders = [
        ("/usr/bin", "backup_bins"),
        ("~", dest),
        ("/var/log", "backup_logs"),
    ]
    base_config["Folders"] = folders
    with NamedTemporaryFile(dir=Path.home()) as src_file:
        fname = f"~/{Path(src_file.name).name}"
        base_config["Files"] = ["/bin/bash", fname]
        cfg = cp.BtrfsConfig.parse_obj(base_config)
    assert Path("~").expanduser() in {src for (src, _) in cfg.Folders}
    assert Path(src_file.name).expanduser() in cfg.Files


@given(
    base_config=valid_unparsed_empty_btrfs_config(),
    folder_dests=st.lists(filenames(), min_size=2, unique=True),
)
def test_btrfs_config_rejects_duplicate_src(base_config, folder_dests: list[str]):
    with TemporaryDirectory() as src:
        folders = [
            ["/usr/bin", "backup_bins"],
            [src, folder_dests[0]],
            ["/var/log", "backup_logs"],
            [src, folder_dests[1]],
        ] + [[src, cur_dest] for cur_dest in folder_dests[2:]]
        base_config["Folders"] = folders
        base_config["Files"] = []
        with pytest.raises(ValidationError, match=re.escape(src)):
            cp.BtrfsConfig.parse_obj(base_config)


@given(
    base_config=valid_unparsed_configs(),
    folder_dest=filenames(),
)
def test_btrfs_config_rejects_duplicate_dest(base_config, folder_dest: str):
    with TemporaryDirectory() as src1:
        with TemporaryDirectory() as src2:
            folders = [
                ["/usr/bin", "backup_bins"],
                [src1, folder_dest],
                ["/var/log", "backup_logs"],
                [src2, folder_dest],
            ]
            base_config["Folders"] = folders
            base_config["Files"] = []
            with pytest.raises(ValidationError, match=re.escape(folder_dest)):
                cp.BtrfsConfig.parse_obj(base_config)
