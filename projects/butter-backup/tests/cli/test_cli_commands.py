import re
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
import storage_device_managers as sdm

from butter_backup import cli
from butter_backup import config_parser as cp
from butter_backup.cli import app

from . import in_docker_container, prepare_config_file


def test_get_default_config_paths(tmp_path: Path, monkeypatch) -> None:
    xdg_config_dir = tmp_path
    monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg_config_dir))
    config_files = cli.get_default_config_paths()
    expected_cfgs = [
        xdg_config_dir / "butter-backup" / "config.json5",
        xdg_config_dir / "butter-backup" / "config.json",
        xdg_config_dir / "butter-backup" / "config.toml",
        xdg_config_dir / "butter-backup" / "config.yaml",
    ]
    assert config_files == expected_cfgs


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_close_does_not_close_unopened_device(runner, encrypted_btrfs_device) -> None:
    config = encrypted_btrfs_device
    with NamedTemporaryFile(suffix=".json") as tempf:
        config_file = Path(tempf.name)
        wrapped_config = cp.Configuration(DeviceConfigurations=[config])
        config_file.write_text(wrapped_config.model_dump_json())
        close_result = runner.invoke(app, ["close", "--config", str(config_file)])
        assert close_result.stdout == ""
        assert close_result.exit_code == 0


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_open_close_roundtrip(runner, encrypted_device) -> None:
    config = encrypted_device
    expected_cryptsetup_map = Path(f"/dev/mapper/{config.UUID}")
    with NamedTemporaryFile(suffix=".json") as tempf:
        config_file = Path(tempf.name)
        wrapped_config = cp.Configuration(DeviceConfigurations=[config])
        config_file.write_text(wrapped_config.model_dump_json())
        open_result = runner.invoke(app, ["open", "--config", str(config_file)])
        expected_msg = (
            f"Speichermedium {config.Name} wurde in (?P<mount_dest>/.+) geöffnet."
        )
        match = re.fullmatch(expected_msg, open_result.stdout.strip())
        assert match is not None
        mount_dest = Path(match.group("mount_dest"))
        assert any(
            mount_dest in destinations
            for destinations in sdm.get_mounted_devices().values()
        )
        assert expected_cryptsetup_map.exists()
        runner.invoke(app, ["close", "--config", str(config_file)])
        assert not expected_cryptsetup_map.exists()
        assert not sdm.is_mounted(mount_dest)
        assert mount_dest.exists()  # Target directory should be kept after closing.


@pytest.mark.parametrize("create_dest_subdir", [True, False])
@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_open_with_explicit_dest(
    runner, encrypted_device, create_dest_subdir: bool, tmp_path: Path
) -> None:
    config = encrypted_device
    expected_cryptsetup_map = Path(f"/dev/mapper/{config.UUID}")
    dest_dir = tmp_path / "mounts"
    dest_dir.mkdir()
    expected_mount_dir = dest_dir / config.Name
    if create_dest_subdir:
        expected_mount_dir.mkdir()
    config_file = tmp_path / "config.json"
    wrapped_config = cp.Configuration(
        DeviceConfigurations=[config], OpenDirectory=dest_dir
    )
    config_file.write_text(wrapped_config.model_dump_json())
    open_result = runner.invoke(app, ["open", "--config", str(config_file)])
    assert open_result.exit_code == 0
    assert str(expected_mount_dir) in open_result.stdout
    assert expected_cryptsetup_map.exists()
    assert expected_mount_dir.exists()
    mount_destinations = sdm.get_mounted_devices()[str(expected_cryptsetup_map)]
    assert expected_mount_dir in mount_destinations
    runner.invoke(app, ["close", "--config", str(config_file)])
    assert not expected_cryptsetup_map.exists()
    assert not sdm.is_mounted(expected_mount_dir)


def test_version(runner) -> None:
    result = runner.invoke(app, ["version"])
    lines = result.stdout.splitlines()
    assert len(lines) == 1
    major, minor, patch = lines[0].split(".")
    assert major.isdecimal()
    assert minor.isdecimal()
    assert patch.isdecimal()


@pytest.mark.parametrize("subprogram", ["open", "backup"])
@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_do_backup_refuses_backup_when_device_is_already_open(
    subprogram: str, runner, encrypted_device, tmp_path: Path
) -> None:
    config_file = prepare_config_file(encrypted_device, tmp_path)
    runner.invoke(app, ["open", "--config", str(config_file)])
    result = runner.invoke(app, [subprogram, "--config", str(config_file)])
    runner.invoke(app, ["close", "--config", str(config_file)])
    expected_msg = f"Speichermedium {encrypted_device.Name} ist bereits geöffnet. Es wird übersprungen."

    assert result.exit_code == 0
    assert expected_msg in result.stderr
