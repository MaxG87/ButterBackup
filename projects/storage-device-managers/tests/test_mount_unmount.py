from __future__ import annotations

import itertools
import typing as t
from pathlib import Path

import pytest
import shell_interface as sh

import storage_device_managers as sdm


class CompressionKwargsT(t.TypedDict):
    compression: sdm.ValidCompressions | None


@pytest.fixture(
    params=[
        {},
        {"compression": sdm.ValidCompressions.ZSTD9},
        {"compression": sdm.ValidCompressions.ZLIB1},
        {"compression": None},
    ]
)
def compression_kwargs(request) -> CompressionKwargsT:
    kwargs: CompressionKwargsT = request.param
    return kwargs


@pytest.fixture(
    params=itertools.product(
        ["ButterBackup", "nested/destination/directory"], [True, False]
    ),
)
def destination(tmp_path: Path, request) -> Path:
    request_param: tuple[str, bool] = request.param
    destination_name, create_destination = request_param
    destination = tmp_path / destination_name
    if create_destination:
        destination.mkdir(parents=True, exist_ok=True)
    return destination


@pytest.mark.parametrize("args", [[], [sdm.ValidCompressions.ZSTD9]])
def in_docker_container() -> bool:
    return Path("/.dockerenv").exists()


class MyCustomTestException(Exception):
    pass


def test_ensure_directory_uses_sudo_for_missing_directory(mocker, tmp_path: Path) -> None:
    destination = tmp_path / "nested" / "mount"
    run_cmd = mocker.patch("storage_device_managers.sh.run_cmd")
    assert sdm.ensure_directory(destination)
    run_cmd.assert_called_once_with(cmd=["sudo", "mkdir", "-p", destination])


def test_ensure_directory_skips_existing_directory(mocker, tmp_path: Path) -> None:
    run_cmd = mocker.patch("storage_device_managers.sh.run_cmd")
    assert not sdm.ensure_directory(tmp_path)
    run_cmd.assert_not_called()


def test_mount_device(
    device_with_fs, tmp_path: Path, compression_kwargs: CompressionKwargsT
) -> None:
    device, _ = device_with_fs
    mount_dir = tmp_path
    sdm.mount_device(device, mount_dir, **compression_kwargs)
    assert sdm.is_mounted(device)
    assert mount_dir in sdm.get_mounted_devices()[str(device)]
    sdm.unmount_device(device)
    assert not sdm.is_mounted(device)


def test_mounted_device(device_with_fs, compression_kwargs: CompressionKwargsT) -> None:
    device, _ = device_with_fs
    with sdm.mounted_device(device, **compression_kwargs) as md:
        assert md.exists()
        assert md.is_dir()
        assert sdm.is_mounted(device)
        assert md in sdm.get_mounted_devices()[str(device)]
    assert not md.exists()
    assert not sdm.is_mounted(device)
    assert str(device) not in sdm.get_mounted_devices()


def test_mounted_device_takes_over_already_mounted_device(
    device_with_fs, tmp_path: Path, compression_kwargs: CompressionKwargsT
) -> None:
    device, _ = device_with_fs
    destination = tmp_path
    sdm.mount_device(device, destination, **compression_kwargs)
    with sdm.mounted_device(device, **compression_kwargs) as md:
        assert sdm.is_mounted(device)
        assert md in sdm.get_mounted_devices()[str(device)]
    assert not sdm.is_mounted(device)


@pytest.mark.skipif(
    in_docker_container(), reason="Root file system may be missing in Docker container."
)
def test_mounted_device_fails_on_not_unmountable_device() -> None:
    def get_root_device() -> Path:
        for device, mount_points in sdm.get_mounted_devices().items():
            if Path("/") in mount_points:
                return Path(device)
        raise ValueError("No device mounted on / was found.")

    root = get_root_device()
    with pytest.raises(sdm.UnmountError), sdm.mounted_device(root):
        pass


def test_mounted_device_unmounts_in_case_of_exception(
    device_with_fs, compression_kwargs: CompressionKwargsT
) -> None:
    device, _ = device_with_fs
    with (
        pytest.raises(MyCustomTestException),
        sdm.mounted_device(device, **compression_kwargs) as md,
    ):
        # That the device is mounted properly is guaranteed by a test
        # above.
        raise MyCustomTestException
    assert not sdm.is_mounted(device), "Device is still mounted after exception."
    assert not md.exists(), "Mounted device still exists after exception."
    assert str(device) not in sdm.get_mounted_devices()


@pytest.mark.parametrize("device", sdm.get_mounted_devices())
def test_is_mounted_detects(device: Path) -> None:
    assert sdm.is_mounted(device)


def test_is_mounted_rejects(tmp_path: Path) -> None:
    assert not sdm.is_mounted(tmp_path)


def test_unmount_device(device_with_fs, tmp_path: Path) -> None:
    device, _ = device_with_fs
    mountpoint = tmp_path
    sdm.mount_device(device, mountpoint)
    sdm.unmount_device(device)
    assert not sdm.is_mounted(device)


def test_unmount_device_raises_unmounterror(tmp_path: Path) -> None:
    # This test calls unmount_device on a Path that is not mounted, which will cause
    # `umount` to fail. On such a failure, unmount_device is expected to raise an
    # UnmountError, which is what this test checks for.
    mountpoint = tmp_path
    with pytest.raises(sdm.UnmountError):
        sdm.unmount_device(mountpoint)


def test_mounted_device_does_not_delete_content_on_umount_error(
    device_with_fs, compression_kwargs: CompressionKwargsT, mocker
) -> None:
    device, _ = device_with_fs
    mocker.patch(
        "storage_device_managers.unmount_device",
        side_effect=sdm.UnmountError("Mocked unmount error"),
    )
    user = sh.get_user()
    sentinel_text = "This file should not be deleted."
    with (
        pytest.raises(sdm.UnmountError, match="Mocked unmount error"),
        sdm.mounted_device(device, **compression_kwargs) as md,
    ):
        sentinel = md / "sentinel-file"
        sdm.chown(md, user, recursive=True)
        sentinel.write_text("This file should not be deleted.")
    assert sentinel.exists(), "Sentinel file was deleted after unmount error."
    assert sentinel.read_text() == sentinel_text
    assert sdm.is_mounted(device)
    mocker.stopall()
    sdm.unmount_device(device)
    assert not sdm.is_mounted(device)


def test_mounted_device_with_destination(
    device_with_fs, destination: Path, compression_kwargs: CompressionKwargsT
) -> None:
    device, _ = device_with_fs
    with sdm.mounted_device(device, destination, **compression_kwargs) as md:
        assert md == destination
        assert sdm.is_mounted(device)
        assert md in sdm.get_mounted_devices()[str(device)]
    assert not sdm.is_mounted(device)
    assert str(device) not in sdm.get_mounted_devices()
    # A given destination should not be deleted after unmounting.
    assert destination.exists()


def test_mounted_device_uses_sudo_to_create_destination(mocker, tmp_path: Path) -> None:
    device = tmp_path / "device"
    destination = tmp_path / "nested" / "mount"
    ensure_directory = mocker.patch(
        "storage_device_managers.ensure_directory", return_value=True
    )
    mocker.patch("storage_device_managers.is_mounted", return_value=False)
    mocker.patch("storage_device_managers.mount_device")
    mocker.patch("storage_device_managers.unmount_device")
    with sdm.mounted_device(device, destination) as mount_dir:
        assert mount_dir == destination
    ensure_directory.assert_called_once_with(destination)
