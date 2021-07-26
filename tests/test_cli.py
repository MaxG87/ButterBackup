from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from unittest import mock

import pytest
from hypothesis import given
from hypothesis import strategies as st
from typer.testing import CliRunner

from butter_backup import __main__ as bb
from butter_backup import cli
from butter_backup.cli import app

path_to_config_files = st.text(st.characters(whitelist_categories="LN"), min_size=1)


@pytest.fixture
def runner():
    return CliRunner(mix_stderr=False)


def test_no_args_parsing() -> None:
    default_config = bb.DEFAULT_CONFIG
    with mock.patch("sys.argv", ["butter-backup"]):
        config = bb.parse_args()
    assert config == default_config


@given(cfg=path_to_config_files, xdg_config=path_to_config_files)
def test_parse_args_returns_passed_file(cfg: str, xdg_config: str) -> None:
    with mock.patch("sys.argv", ["butter-backup", "--config", cfg]):
        with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config}.get):
            parsed_config = bb.parse_args()
    assert Path(cfg) == parsed_config


@given(xdg_config=path_to_config_files)
def test_parse_args_returns_xdg_config_home(xdg_config: str) -> None:
    with mock.patch("sys.argv", ["butter-backup"]):
        with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config}.get):
            parsed_config = bb.parse_args()
    assert Path(xdg_config) / bb.DEFAULT_CONFIG_NAME == parsed_config


def test_get_default_config_path_refuses_missing_xdg_config() -> None:
    with TemporaryDirectory() as tempdir:
        xdg_config_dir = Path(tempdir)
    with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config_dir}.get):
        with pytest.raises(SystemExit) as exc:
            cli.get_default_config_path()
    expected_cfg_file_name = xdg_config_dir / cli.DEFAULT_CONFIG_NAME
    assert f"{expected_cfg_file_name}" in exc.value.args[0]


def test_start_click_cli() -> None:
    runner = CliRunner()
    result = runner.invoke(app, ["hilfe"])
    assert "Hilfe!" in result.stdout
    assert result.exit_code == 0


def test_open_refuses_missing_config(runner) -> None:
    with NamedTemporaryFile() as file:
        config_file = Path(file.name)
    result = runner.invoke(app, ["open", "--config", str(config_file)])
    assert f"{config_file}" in result.stderr
    assert result.exit_code != 0


def test_open_opens_device(runner, encrypted_btrfs_device) -> None:
    password, device = encrypted_btrfs_device
    assert password != ""
