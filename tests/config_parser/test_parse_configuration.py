import json
import typing as t
from pathlib import Path
from tempfile import TemporaryDirectory

import pytest
from hypothesis import given
from hypothesis import strategies as st
from pydantic import ValidationError

from butter_backup import config_parser as cp
from tests import hypothesis_utils as hu

SUCCESS_CODES = {0, None}

EXAMPLES_DIR = Path(__file__).parent.parent.parent / "examples"


@pytest.mark.parametrize(
    "example_file",
    [
        EXAMPLES_DIR / "json.cfg",
        EXAMPLES_DIR / "json5.cfg",
        EXAMPLES_DIR / "toml.cfg",
        EXAMPLES_DIR / "yaml.cfg",
    ],
)
def test_example_files_can_be_parsed(example_file: Path) -> None:
    content = example_file.read_text()
    result = cp.parse_configuration(content)
    expected_names = ["BtrFS Backup Example", "Restic Backup Example"]
    assert [cfg.Name for cfg in result.deviceConfigurations] == expected_names


@given(
    first_config=hu.valid_empty_btrfs_config(None) | hu.valid_empty_restic_config(None),
    second_config=hu.valid_empty_btrfs_config(None)
    | hu.valid_empty_restic_config(None),
    shared_name=hu.valid_path_components(),
)
def test_parse_configuration_rejects_duplicate_names(
    first_config: cp.BtrFSRsyncConfig | cp.ResticConfig,
    second_config: cp.BtrFSRsyncConfig | cp.ResticConfig,
    shared_name: str,
) -> None:
    first_config = first_config.model_copy(update={"Name": shared_name})
    second_config = second_config.model_copy(update={"Name": shared_name})
    with pytest.raises(ValidationError):
        cp.parse_configuration(
            json.dumps(
                {
                    "deviceConfigurations": [
                        first_config.model_dump(mode="json"),
                        second_config.model_dump(mode="json"),
                    ]
                }
            )
        )


def test_parse_configuration_rejects_empty_list() -> None:
    with pytest.raises(SystemExit) as sysexit:
        cp.parse_configuration(json.dumps({"deviceConfigurations": []}))
    assert sysexit.value.code not in SUCCESS_CODES


@given(
    non_list=st.one_of(
        st.dictionaries(st.text(), st.text(), min_size=1), st.text(min_size=1)
    )
)
def test_parse_configuration_warns_on_non_lists(non_list) -> None:
    with pytest.raises(ValidationError):
        cp.parse_configuration(json.dumps({"deviceConfigurations": non_list}))


def test_parse_configuration_warns_on_non_dict_item() -> None:
    with pytest.raises(ValidationError):
        cp.parse_configuration(json.dumps({"deviceConfigurations": [{}, 1337]}))


@given(
    base_config=hu.valid_unparsed_empty_btrfs_config(None),
    files_folders_dest=st.lists(
        hu.valid_path_components(), min_size=2, max_size=2, unique=True
    ),
)
def test_parse_configuration_parses_btrfs_config(
    base_config: dict[str, t.Any], files_folders_dest: list[str]
) -> None:
    files_dest, folders_dest = files_folders_dest
    with TemporaryDirectory() as source:
        base_config.update({"Folders": {source: folders_dest}, "FilesDest": files_dest})
        btrfs_cfg = cp.BtrFSRsyncConfig.model_validate(base_config)
        cfg = cp.Configuration(deviceConfigurations=[btrfs_cfg])
        result = cp.parse_configuration(cfg.model_dump_json())
        assert result == cfg


@given(
    base_config=hu.valid_unparsed_empty_restic_config(None),
)
def test_load_configuration_parses_restic_config(base_config: dict[str, t.Any]) -> None:
    with TemporaryDirectory() as source:
        base_config["FilesAndFolders"] = [source]
        restic_cfg = cp.ResticConfig.model_validate(base_config)
        cfg = cp.Configuration(deviceConfigurations=[restic_cfg])
        result = cp.parse_configuration(cfg.model_dump_json())
        assert result == cfg


@given(
    device_configurations=st.lists(
        hu.valid_empty_restic_config(None) | hu.valid_empty_btrfs_config(None),
        min_size=1,
        unique_by=lambda cfg: cfg.Name,
    ),
    sudo_pass_cmd=st.text(),
)
def test_parse_configuration_with_sudo_pass_cmd(
    device_configurations: list[cp.ResticConfig | cp.BtrFSRsyncConfig],
    sudo_pass_cmd: str,
) -> None:
    cfg = cp.Configuration(
        deviceConfigurations=device_configurations,
        SudoPassCmd=sudo_pass_cmd,
    )
    raw = cfg.model_dump_json()
    result = cp.parse_configuration(raw)
    assert result == cfg
