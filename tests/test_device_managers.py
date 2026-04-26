from pathlib import Path

import pytest
import storage_device_managers as sdm

from butter_backup import device_managers as dm


def in_docker_container() -> bool:
    return Path("/.dockerenv").exists()


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_open_encrypted_device_raises_device_pass_cmd_error_on_failing_pass_cmd(
    encrypted_device,
) -> None:
    """When DevicePassCmd fails, DevicePassCmdError must be raised, not DeviceDecryptionError."""
    device = encrypted_device.device()
    failing_pass_cmd = "false"  # always exits with non-zero status

    with pytest.raises(dm.DevicePassCmdError):
        dm.open_encrypted_device(device, failing_pass_cmd)


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_decrypted_device_raises_device_pass_cmd_error_on_failing_pass_cmd(
    encrypted_device,
) -> None:
    """When DevicePassCmd fails, DevicePassCmdError must be raised, not DeviceDecryptionError."""
    device = encrypted_device.device()
    failing_pass_cmd = "false"  # always exits with non-zero status

    with pytest.raises(dm.DevicePassCmdError):
        with dm.decrypted_device(device, failing_pass_cmd):
            pass  # pragma: no cover


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_open_encrypted_device_raises_device_decryption_error_on_wrong_password(
    encrypted_device,
) -> None:
    """When DevicePassCmd succeeds but the password is wrong, DeviceDecryptionError must be raised."""
    device = encrypted_device.device()
    wrong_password_cmd = "echo wrong_password"

    with pytest.raises(sdm.DeviceDecryptionError):
        dm.open_encrypted_device(device, wrong_password_cmd)
