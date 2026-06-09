from __future__ import annotations

from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import call

import shell_interface as sh

import storage_device_managers as sdm


def test_sync_device_is_called_on_unmount(btrfs_device, mocker) -> None:
    spy = mocker.spy(sdm, "sync_device")
    with TemporaryDirectory() as mount_dir:
        sdm.mount_btrfs_device(btrfs_device, Path(mount_dir))
        sdm.unmount_device(btrfs_device)
    spy.assert_called_once_with(btrfs_device)


def test_sync_device_calls_sync_f_for_btrfs(btrfs_device, mocker) -> None:
    with TemporaryDirectory() as mount_dir:
        sdm.mount_btrfs_device(btrfs_device, Path(mount_dir))
        spy = mocker.spy(sh, "run_cmd")
        sdm.sync_device(btrfs_device)
        sh.run_cmd(cmd=["sudo", "umount", btrfs_device])
    assert call(cmd=["sudo", "sync", "-f", btrfs_device]) in spy.call_args_list


def test_sync_device_calls_btrfs_filesystem_sync_for_btrfs(
    btrfs_device, mocker
) -> None:
    with TemporaryDirectory() as mount_dir:
        mount_path = Path(mount_dir)
        sdm.mount_btrfs_device(btrfs_device, mount_path)
        spy = mocker.spy(sh, "run_cmd")
        sdm.sync_device(btrfs_device)
        sh.run_cmd(cmd=["sudo", "umount", btrfs_device])
    assert (
        call(cmd=["sudo", "btrfs", "filesystem", "sync", mount_path])
        in spy.call_args_list
    )


def test_sync_device_calls_sync_f_for_ext4(ext4_device, mocker) -> None:
    with TemporaryDirectory() as mount_dir:
        sdm.mount_ext4_device(ext4_device, Path(mount_dir))
        spy = mocker.spy(sh, "run_cmd")
        sdm.sync_device(ext4_device)
        sh.run_cmd(cmd=["sudo", "umount", ext4_device])
    assert call(cmd=["sudo", "sync", "-f", ext4_device]) in spy.call_args_list


def test_sync_device_does_not_call_btrfs_sync_for_ext4(ext4_device, mocker) -> None:
    with TemporaryDirectory() as mount_dir:
        sdm.mount_ext4_device(ext4_device, Path(mount_dir))
        spy = mocker.spy(sh, "run_cmd")
        sdm.sync_device(ext4_device)
        sh.run_cmd(cmd=["sudo", "umount", ext4_device])
    btrfs_calls = [c for c in spy.call_args_list if "btrfs" in str(c)]
    assert not btrfs_calls
