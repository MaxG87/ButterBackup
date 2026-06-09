from __future__ import annotations

import storage_device_managers as sdm


def test_get_filesystem(device_with_fs) -> None:
    (device, expected_filesystem) = device_with_fs
    assert sdm.get_filesystem(device) == expected_filesystem
