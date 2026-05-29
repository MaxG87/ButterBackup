import os
import typing as t
from pathlib import Path

import pytest
import shell_interface as sh
from click.testing import Result
from typer.testing import CliRunner

from butter_backup import config_parser as cp
from butter_backup.cli import app
from tests import complement_configuration

from . import in_docker_container, prepare_tmp_path

_SUDO_PASS_CMD = os.environ.get("BUTTERBACKUP_SUDO_PASSCMD")
_requires_sudo_pass_cmd = pytest.mark.skipif(
    _SUDO_PASS_CMD is None,
    reason="BUTTERBACKUP_SUDO_PASSCMD environment variable is not set",
)


@pytest.fixture
def runner():
    return CliRunner()


def _invalidate_sudo_session() -> None:
    sh.run_cmd(cmd=["sudo", "-k"])


def test_sudo_pass_cmd_is_used_in_open(
    runner: CliRunner,
    encrypted_device: cp.DeviceConfiguration,
    mocker,
    tmp_path: Path,
) -> None:
    sudo_pass_cmd = "echo test_password"
    wrapped_config = cp.Configuration(
        DeviceConfigurations=[encrypted_device], SudoPassCmd=sudo_pass_cmd
    )
    config_file = tmp_path / "config.json"
    config_file.write_text(wrapped_config.model_dump_json())

    mock_pipe = mocker.patch("shell_interface.pipe_pass_cmd_to_real_cmd")
    mocker.patch(
        "storage_device_managers.open_encrypted_device",
        return_value=Path("/dev/mapper/test"),
    )
    mocker.patch("storage_device_managers.mount_device")

    runner.invoke(app, ["open", "--config", str(config_file)])

    mock_pipe.assert_called_once_with(
        sudo_pass_cmd, ["sudo", "-Sv"], capture_output=True
    )


def test_sudo_pass_cmd_is_used_in_backup(
    runner: CliRunner,
    encrypted_device: cp.DeviceConfiguration,
    mocker,
    tmp_path: Path,
) -> None:
    sudo_pass_cmd = "echo test_password"
    wrapped_config = cp.Configuration(
        DeviceConfigurations=[encrypted_device], SudoPassCmd=sudo_pass_cmd
    )
    config_file = tmp_path / "config.json"
    config_file.write_text(wrapped_config.model_dump_json())

    mock_pipe = mocker.patch("shell_interface.pipe_pass_cmd_to_real_cmd")
    mocker.patch("storage_device_managers.decrypted_device")
    mocker.patch("storage_device_managers.mounted_device")
    mocker.patch(
        "butter_backup.backup_backends.BackupBackend.from_config",
        return_value=mocker.MagicMock(),
    )

    runner.invoke(app, ["backup", "--config", str(config_file)])

    expected_nof_calls = 2  # One before opening the device and one post backup
    result_calls = mock_pipe.call_args_list
    expected_calls = [
        ((sudo_pass_cmd, ["sudo", "-Sv"]), {"capture_output": True})
    ] * expected_nof_calls
    assert result_calls == expected_calls


def test_sudo_pass_cmd_is_used_in_close(
    runner: CliRunner,
    encrypted_device: cp.DeviceConfiguration,
    mocker,
    tmp_path: Path,
) -> None:
    sudo_pass_cmd = "echo test_password"
    wrapped_config = cp.Configuration(
        DeviceConfigurations=[encrypted_device], SudoPassCmd=sudo_pass_cmd
    )
    config_file = tmp_path / "config.json"
    config_file.write_text(wrapped_config.model_dump_json())
    map_name = str(encrypted_device.map_name())

    mock_pipe = mocker.patch("shell_interface.pipe_pass_cmd_to_real_cmd")
    mocker.patch(
        "storage_device_managers.get_mounted_devices",
        return_value={map_name: [tmp_path / "mnt"]},
    )
    mocker.patch("storage_device_managers.unmount_device")
    mocker.patch("storage_device_managers.close_decrypted_device")

    runner.invoke(app, ["close", "--config", str(config_file)])

    mock_pipe.assert_called_once_with(
        sudo_pass_cmd, ["sudo", "-Sv"], capture_output=True
    )


@_requires_sudo_pass_cmd
@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
@pytest.mark.parametrize(
    "command,has_failed",
    [
        (
            "open",
            lambda config, result: (
                f"Speichermedium {config.Name} konnte nicht geöffnet werden."
                in result.stdout
            ),
        ),
        ("backup", lambda _, result: result.exit_code != 0),
        ("close", lambda _, result: result.exit_code != 0),
    ],
)
def test_open_requires_correct_sudo_pass_cmd(
    runner: CliRunner,
    encrypted_device: cp.DeviceConfiguration,
    command: str,
    has_failed: t.Callable[[cp.DeviceConfiguration, Result], bool],
    tmp_path: Path,
) -> None:
    assert _SUDO_PASS_CMD is not None
    config = complement_configuration(encrypted_device, tmp_path)
    prepare_tmp_path(config, tmp_path)
    correct_config = cp.Configuration(
        DeviceConfigurations=[config], SudoPassCmd=_SUDO_PASS_CMD
    )
    correct_config_file = tmp_path / "correct_config.json"
    correct_config_file.write_text(correct_config.model_dump_json())
    wrong_config = cp.Configuration(DeviceConfigurations=[config], SudoPassCmd="false")
    wrong_config_file = tmp_path / "wrong_config.json"
    wrong_config_file.write_text(wrong_config.model_dump_json())

    # Wrong pass-cmd: pipe_pass_cmd_to_real_cmd raises PassCmdError since "false"
    # exits 1. The open command catches it and prints the failure message.
    _invalidate_sudo_session()
    if command == "close":
        # The device needs to be open for the close command to pick up the device at
        # all.
        runner.invoke(app, ["open", "--config", str(correct_config_file)])

    wrong_result = runner.invoke(app, [command, "--config", str(wrong_config_file)])
    assert has_failed(config, wrong_result)

    # Correct password: open should succeed
    _invalidate_sudo_session()
    correct_result = runner.invoke(app, [command, "--config", str(correct_config_file)])
    assert correct_result.exit_code == 0

    # Cleanup
    runner.invoke(app, ["close", "--config", str(correct_config_file)])
