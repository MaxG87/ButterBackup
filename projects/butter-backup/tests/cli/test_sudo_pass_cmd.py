import os
import subprocess
import typing as t
from pathlib import Path

import pytest
import shell_interface as sh
from typer.testing import CliRunner, Result

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


@pytest.fixture
def root_owned_tmp_path(tmp_path: Path) -> t.Iterable[Path]:
    """
    Create a temporary directory owned by root for testing
    """
    root_owned_path = tmp_path / "root_owned"
    root_owned_path.mkdir()
    current_user = sh.get_user()
    current_group = sh.get_group(current_user)
    sh.chown(root_owned_path, "root", "root", recursive=False)
    try:
        yield root_owned_path
    finally:
        sh.chown(root_owned_path, current_user, current_group, recursive=False)


def _assert_sudo_refresh_occurred_before_privileged_cmd(
    refresh_idx: list[int], other_sudo_idx: list[int]
) -> None:
    # Ensure that the sudo refresh and other privileged commands occurred alternating.
    # Ideally, we would check that no two refreshes occurred without a privileged
    # command in between, but this is believed to be too strict. In the test setup, some
    # privileged commands may be skipped, e.g. because there is nothing to back up.
    assert len(refresh_idx) > 0
    assert len(other_sudo_idx) > 0
    assert min(refresh_idx) < min(other_sudo_idx)
    assert max(refresh_idx) < max(other_sudo_idx)


def _invalidate_sudo_session() -> None:
    sh.run_cmd(cmd=["sudo", "-k"])


def _is_sudo_refresh(call: t.Any) -> bool:
    ret: bool = call.args and call.args[0][:2] == ["sudo", "-Sv"]
    return ret


def _is_non_refresh_sudo_cmd(call: t.Any) -> bool:
    ret: bool = call.args and call.args[0][0] == "sudo" and not _is_sudo_refresh(call)
    return ret


def test_sudo_pass_cmd_is_used_in_open(
    runner: CliRunner,
    encrypted_device: cp.DeviceConfiguration,
    mocker,
    tmp_path: Path,
) -> None:
    sudo_pass_cmd = "echo test_password"
    dest_dir = tmp_path / "mounts"
    dest_dir.mkdir()
    wrapped_config = cp.Configuration(
        DeviceConfigurations=[encrypted_device],
        OpenDirectory=dest_dir,
        SudoPassCmd=sudo_pass_cmd,
    )
    config_file = tmp_path / "config.json"
    config_file.write_text(wrapped_config.model_dump_json())

    spy = mocker.spy(subprocess, "run")

    open_result = runner.invoke(app, ["open", "--config", str(config_file)])
    assert open_result.exit_code == 0, open_result.output

    sudo_refresh_calls = [
        i for i, c in enumerate(spy.call_args_list) if _is_sudo_refresh(c)
    ]
    other_privileged_calls = [
        i for i, c in enumerate(spy.call_args_list) if _is_non_refresh_sudo_cmd(c)
    ]
    assert sudo_refresh_calls and other_privileged_calls
    assert max(sudo_refresh_calls) < min(other_privileged_calls)


def test_open_uses_sudo_to_create_mount_dir(
    runner: CliRunner,
    encrypted_btrfs_device: cp.DeviceConfiguration,
    tmp_path: Path,
    root_owned_tmp_path: Path,
) -> None:
    dest_dir = root_owned_tmp_path / "mounts"
    wrapped_config = cp.Configuration(
        DeviceConfigurations=[encrypted_btrfs_device],
        OpenDirectory=dest_dir,
    )
    config_file = tmp_path / "config.json"
    config_file.write_text(wrapped_config.model_dump_json())

    result = runner.invoke(app, ["open", "--config", str(config_file)])
    assert result.exit_code == 0
    assert dest_dir.exists()
    runner.invoke(app, ["close", "--config", str(config_file)])


def test_sudo_session_is_refreshed_around_backup(
    runner: CliRunner,
    encrypted_device: cp.DeviceConfiguration,
    mocker,
    tmp_path: Path,
) -> None:
    sudo_pass_cmd = "echo test_password"
    wrapped_config = cp.Configuration(
        DeviceConfigurations=[encrypted_device], SudoPassCmd=sudo_pass_cmd
    )

    match encrypted_device:
        case cp.BtrFSRsyncConfig():
            expected_nof_refreshes = 3
        case cp.ResticConfig():
            expected_nof_refreshes = 4
        case _:
            t.assert_never(encrypted_device)

    config_file = tmp_path / "config.json"
    config_file.write_text(wrapped_config.model_dump_json())

    spy = mocker.spy(subprocess, "run")
    result = runner.invoke(app, ["backup", "--config", str(config_file)])
    assert result.exit_code == 0, result.output

    refresh_idx = [i for i, c in enumerate(spy.call_args_list) if _is_sudo_refresh(c)]
    other_idx = [
        i for i, c in enumerate(spy.call_args_list) if _is_non_refresh_sudo_cmd(c)
    ]

    assert len(refresh_idx) == expected_nof_refreshes
    _assert_sudo_refresh_occurred_before_privileged_cmd(refresh_idx, other_idx)


def test_sudo_session_is_refreshed_before_close(
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

    open_result = runner.invoke(app, ["open", "--config", str(config_file)])
    assert open_result.exit_code == 0, open_result.output
    spy = mocker.spy(subprocess, "run")
    close_result = runner.invoke(app, ["close", "--config", str(config_file)])
    assert close_result.exit_code == 0, close_result.output

    refresh_idx = [i for i, c in enumerate(spy.call_args_list) if _is_sudo_refresh(c)]
    other_idx = [
        i for i, c in enumerate(spy.call_args_list) if _is_non_refresh_sudo_cmd(c)
    ]

    assert len(refresh_idx) == 1
    _assert_sudo_refresh_occurred_before_privileged_cmd(refresh_idx, other_idx)


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
