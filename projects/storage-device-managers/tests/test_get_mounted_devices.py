from __future__ import annotations

from pathlib import Path

import pytest

import storage_device_managers as sdm


def test_get_mounted_devices_raises_on_unknown_device() -> None:
    with pytest.raises(KeyError):
        sdm.get_mounted_devices()["unknown-device"]


def test_get_mounted_devices_includes_correct_mountpoints(mounted_directories) -> None:
    _, dest = mounted_directories
    all_mounts = sdm.get_mounted_devices()
    for dest_set in all_mounts.values():
        if dest in dest_set:
            mount_options = dest_set[dest]
            break
    else:
        raise AssertionError(f"Expected mount point {dest} not found.")
    assert "rw" in mount_options


def test_get_mounted_devices_includes_root() -> None:
    assert any(Path("/") in dest_set for dest_set in sdm.get_mounted_devices().values())
