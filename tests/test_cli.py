import json
import uuid
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from unittest import mock

import pytest
from typer.testing import CliRunner

from butter_backup import cli
from butter_backup import config_parser as cp
from butter_backup import device_managers as dm
from butter_backup.cli import app


@pytest.fixture
def runner():
    return CliRunner(mix_stderr=False)


def test_get_default_config_path_refuses_missing_xdg_config() -> None:
    with TemporaryDirectory() as tempdir:
        xdg_config_dir = Path(tempdir)
    with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config_dir}.get):
        with pytest.raises(SystemExit) as exc:
            cli.get_default_config_path()
    expected_cfg_file_name = xdg_config_dir / cli.DEFAULT_CONFIG_NAME
    assert f"{expected_cfg_file_name}" in exc.value.args[0]


def test_backup_refuses_missing_config(runner) -> None:
    with NamedTemporaryFile() as file:
        config_file = Path(file.name)
    result = runner.invoke(app, ["backup", "--config", str(config_file)])
    assert f"{config_file}" in result.stderr
    assert result.exit_code != 0


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
    device_id = str(uuid.uuid4())
    config = cp.ParsedButterConfig(
        files=set(),
        files_dest="files-destination",
        folders=set(),
        pass_cmd=f"echo {password}",
        uuid=device_id,
    )
    with NamedTemporaryFile() as tempf:
        config_file = Path(tempf.name)
        config_file.write_text(json.dumps(config.as_dict()))
        dest = Path("/dev/disk/by-uuid/") / device_id
        with dm.symbolic_link(src=device, dest=dest):
            pass
    assert password != ""
