import re
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


def in_docker_container() -> bool:
    return Path("/.dockerenv").exists()


@pytest.fixture
def runner():
    return CliRunner(mix_stderr=False)


def test_get_default_config_path() -> None:
    with TemporaryDirectory() as tempdir:
        xdg_config_dir = Path(tempdir)
    with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config_dir}.get):
        config_file = cli.get_default_config_path()
    expected_cfg = xdg_config_dir / cli.DEFAULT_CONFIG_NAME
    assert str(expected_cfg) == config_file


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


@pytest.mark.skip("Impossible to implement!")
def test_open_refuses_missing_xdg_config(runner) -> None:
    # It seems as if this test cannot be implemented at the moment.
    #
    # This test resets XDG_CONFIG_HOME to provoke that get_default_config_path
    # returns a not existing config file. However, get_default_config_path is
    # executed at import time, rendering resetting XDG_CONFIG_HOME effectless.
    with TemporaryDirectory() as xdg_config_dir:
        pass
    with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config_dir}.get):
        result = runner.invoke(app, ["open"])
    assert xdg_config_dir in result.stderr
    assert result.exit_code != 0


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_close_does_not_close_unopened_device(runner, encrypted_btrfs_device) -> None:
    password, device = encrypted_btrfs_device
    device_id = uuid.uuid4()
    config = cp.BtrfsConfig(
        Files=set(),
        FilesDest="files-destination",
        Folders={},
        PassCmd=f"echo {password}",
        UUID=device_id,
    )
    with NamedTemporaryFile() as tempf:
        config_file = Path(tempf.name)
        config_file.write_text(f"[{config.json()}]")
        device_by_uuid = Path("/dev/disk/by-uuid/") / str(device_id)
        with dm.symbolic_link(src=device, dest=device_by_uuid):
            close_result = runner.invoke(app, ["close", "--config", str(config_file)])
            assert close_result.stdout == ""
            assert close_result.exit_code == 0


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_open_close_roundtrip(runner, encrypted_btrfs_device) -> None:
    password, device = encrypted_btrfs_device
    device_id = uuid.uuid4()
    expected_cryptsetup_map = Path(f"/dev/mapper/{device_id}")
    config = cp.BtrfsConfig(
        Files=set(),
        FilesDest="files-destination",
        Folders={},
        PassCmd=f"echo {password}",
        UUID=device_id,
    )
    with NamedTemporaryFile() as tempf:
        config_file = Path(tempf.name)
        config_file.write_text(f"[{config.json()}]")
        device_by_uuid = Path("/dev/disk/by-uuid/") / str(device_id)
        with dm.symbolic_link(src=device, dest=device_by_uuid):
            open_result = runner.invoke(app, ["open", "--config", str(config_file)])
            expected_msg = (
                f"Gerät {device_id} wurde in (?P<mount_dest>/[^ ]+) geöffnet."
            )
            match = re.fullmatch(expected_msg, open_result.stdout.strip())
            assert match is not None
            mount_dest = Path(match.group("mount_dest"))
            assert any(
                mount_dest in destinations
                for destinations in dm.get_mounted_devices().values()
            )
            assert expected_cryptsetup_map.exists()
            runner.invoke(app, ["close", "--config", str(config_file)])
            assert not expected_cryptsetup_map.exists()
            assert not dm.is_mounted(mount_dest)
            assert not mount_dest.exists()
