import re
import typing as t
from contextlib import contextmanager
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
import storage_device_managers as sdm

from butter_backup import cli
from butter_backup import config_parser as cp
from butter_backup.cli import app
from tests import get_random_filename

from . import in_docker_container, prepare_config_file


def _assert_is_error_result(result, expected_exit_code: int = 1) -> None:
    assert result.exit_code == expected_exit_code
    assert isinstance(result.exception, SystemExit)


def _assert_output_is_single_line_errmsg(
    result, expected_snippets: set[str], prohibited_snippets: set[str] | None = None
) -> None:
    stderr_lines = result.stderr.splitlines()
    stdout_lines = result.stdout.splitlines()
    prohibited_snippets = prohibited_snippets or set()
    assert stdout_lines == []
    assert len(stderr_lines) == 1
    assert all(snippet in stderr_lines[0] for snippet in expected_snippets)
    assert all(snippet not in stderr_lines[0] for snippet in prohibited_snippets)


@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_missing_config(subprogram, runner) -> None:
    config_file = Path(get_random_filename())
    result = runner.invoke(app, [subprogram, "--config", str(config_file)])
    assert f"{config_file}" in result.stderr
    _assert_is_error_result(result, expected_exit_code=2)


@pytest.mark.skipif(in_docker_container(), reason="All files are readable for root")
@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_unreadable_file(subprogram, runner) -> None:
    with NamedTemporaryFile(suffix=".json") as fh:
        config_file = Path(fh.name)
        config_file.chmod(0)
        result = runner.invoke(app, [subprogram, "--config", str(config_file)])
        assert f"{config_file}" in result.stderr
        _assert_is_error_result(result, expected_exit_code=2)


@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_directories(subprogram, runner, tmp_path: Path) -> None:
    tmp_path_as_str = str(tmp_path)
    result = runner.invoke(app, [subprogram, "--config", tmp_path_as_str])
    assert tmp_path_as_str in result.stderr
    _assert_is_error_result(result, expected_exit_code=2)


def test_open_refuses_missing_xdg_config(runner, tmp_path, monkeypatch) -> None:
    xdg_config_dir = tmp_path / "nonexistent_config_dir"
    monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg_config_dir))
    result = runner.invoke(app, ["open"])
    assert str(xdg_config_dir) in result.stderr
    _assert_is_error_result(result, expected_exit_code=2)


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_open_shows_error_on_failure(runner, encrypted_device, tmp_path: Path) -> None:
    # Use a wrong passphrase so that decryption fails naturally without any mocking.
    config = encrypted_device.model_copy(
        update={"DevicePassCmd": "echo wrong_password"}
    )
    dest_dir = tmp_path / "mounts"
    dest_dir.mkdir()
    config_file = tmp_path / "config.json"
    wrapped_config = cp.Configuration(
        DeviceConfigurations=[config], OpenDirectory=dest_dir
    )
    config_file.write_text(wrapped_config.model_dump_json())
    open_result = runner.invoke(app, ["open", "--config", str(config_file)])
    expected_msg = f"Speichermedium {config.Name} konnte nicht geöffnet werden. Es wird übersprungen."
    assert open_result.exit_code == 0
    assert expected_msg in open_result.stdout
    # The empty mount dir should have been cleaned up after the failure
    assert not (dest_dir / config.Name).exists()


def test_unmount_error_does_not_cause_content_deletion(
    runner, encrypted_device: cp.DeviceConfiguration, tmp_path: Path, mocker
) -> None:
    # THIS IS A REGRESSION TEST!
    #
    # A previous version of the code had a serious bug, where a failed unmount operation
    # would cause the content of the mount point (i.e. **the backups**!) to be deleted.
    # This test ensures that this bug is fixed by provoking an unmount error and
    # checking that:
    #
    # 1. Unmounting indeed failed (exit_code == 1).
    # 2. The backup repository still exists after the failed unmount operation
    # 3. The device can be closed successfully after the failed unmount operation
    #
    # This test "successfully" provoked the buggy behaviour before the bug was fixed.
    mocker.patch(
        "storage_device_managers.unmount_device",
        side_effect=sdm.UnmountError(
            ["sudo", "umount", encrypted_device.map_name()], b"Mocked stderr"
        ),
    )

    config_file = prepare_config_file(encrypted_device, tmp_path)

    backup_result = runner.invoke(app, ["backup", "--config", str(config_file)])
    # Check that BackupRepositoryFolder still exists after the failed unmount operation.
    # It is assumed that the device is still mounted, since the unmounting is mocked to
    # fail.
    mounts = sdm.get_mounted_devices()
    mount_of_device = next(iter(mounts[str(encrypted_device.map_name())]))
    expected_backup_repository = (
        mount_of_device / encrypted_device.BackupRepositoryFolder
    )
    # Cannot use assert here, as failure would prevent the clean-up code below from
    # running.
    expected_backup_repository_exists = expected_backup_repository.exists()
    expected_backup_repository_is_dir = expected_backup_repository.is_dir()

    # Clean-up: Check that the device can be closed successfully after the failed
    # unmount operation.
    mocker.stopall()
    close_result = runner.invoke(app, ["close", "--config", str(config_file)])
    _assert_is_error_result(backup_result, expected_exit_code=1)
    assert close_result.exit_code == 0
    assert expected_backup_repository_exists
    assert expected_backup_repository_is_dir
    assert mount_of_device.exists()  # Target directory should be kept after closing.
    assert sdm.is_mounted(mount_of_device) is False


def test_incorrect_backup_repository_field_has_explicit_log_message(
    runner, mocker, encrypted_device, tmp_path
) -> None:
    incorrect_folder_name = "FolderThatDoesNotExist"
    config_file = prepare_config_file(encrypted_device, tmp_path)
    config = cli._read_configuration(config_file)
    broken_device_config = encrypted_device.model_copy(
        update={"BackupRepositoryFolder": incorrect_folder_name}
    )
    broken_config = config.model_copy(
        update={"DeviceConfigurations": [broken_device_config]}
    )
    config_file.write_text(broken_config.model_dump_json())

    result = runner.invoke(app, ["backup", "--config", str(config_file)])

    assert not encrypted_device.map_name().exists()  # Device closed successfully
    _assert_is_error_result(result, expected_exit_code=1)

    # Check STDERR
    _assert_output_is_single_line_errmsg(
        result,
        {incorrect_folder_name, encrypted_device.BackupRepositoryFolder},
        {"PosixPath"},
    )


def test_close_handles_unmount_error_correctly(
    runner, mocker, encrypted_device, tmp_path
) -> None:
    config_file = prepare_config_file(encrypted_device, tmp_path)

    result = runner.invoke(app, ["open", "--config", str(config_file)])
    assert result.exit_code == 0

    # Hook in right after mounting to keep a file handle open, forcing a
    # real unmount failure once the CLI tries to clean up.
    original_unmount_device = sdm.unmount_device

    @contextmanager
    def _failing_unmount_device(device: Path):
        raise sdm.UnmountError(["sudo", "umount", device], b"Mocked stderr")

    mocker.patch.object(sdm, "unmount_device", _failing_unmount_device)
    failing_result = runner.invoke(app, ["close", "--config", str(config_file)])
    mocker.patch.object(sdm, "unmount_device", original_unmount_device)
    result = runner.invoke(app, ["close", "--config", str(config_file)])
    _assert_is_error_result(failing_result, expected_exit_code=1)
    assert result.exit_code == 0

    stderr_lines = failing_result.stderr.splitlines()
    assert result.stdout == ""
    assert len(stderr_lines) == 1
    assert re.match(
        "Aushängen des Speichermediums .* ist fehlgeschlagen. Die Fehlermeldung ist:",
        stderr_lines[0],
    )


def test_backup_handles_unmount_error_correctly(
    runner, mocker, encrypted_device, tmp_path
) -> None:
    config_file = prepare_config_file(encrypted_device, tmp_path)

    # Hook in right after mounting to keep a file handle open, forcing a
    # real unmount failure once the CLI tries to clean up.
    original_mounted_device = sdm.mounted_device
    captured_blocker: t.IO[str] | None = None

    @contextmanager
    def _busy_mounted_device(*args, **kwargs):
        nonlocal captured_blocker
        with original_mounted_device(*args, **kwargs) as mount_point:
            captured_blocker = open(mount_point / "busy-marker", "w")  # noqa: SIM115
            yield mount_point

    mocker.patch.object(sdm, "mounted_device", _busy_mounted_device)
    # result = runner.invoke(app, ["backup"])
    backup_result = runner.invoke(app, ["backup", "--config", str(config_file)])

    # Clean-up
    assert captured_blocker is not None
    captured_blocker.close()
    close_result = runner.invoke(app, ["close", "--config", str(config_file)])
    _assert_is_error_result(backup_result, expected_exit_code=1)
    assert close_result.exit_code == 0

    assert backup_result.stdout == ""
    assert re.match(
        "Aushängen des Speichermediums .* ist fehlgeschlagen. Die Fehlermeldung ist:",
        backup_result.stderr,
    )
