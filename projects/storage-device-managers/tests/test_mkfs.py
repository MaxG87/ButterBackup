from __future__ import annotations

import typing as t

import pytest

import storage_device_managers as sdm


def test_mkfs_ext4(big_file) -> None:
    sdm.mkfs_ext4(big_file)
    assert sdm.get_filesystem(big_file) == "ext4"


@pytest.mark.parametrize("filesystem", t.get_args(sdm.ValidFileSystems))
def test_mkfs(big_file, filesystem) -> None:
    sdm.mkfs(big_file, filesystem)
    assert sdm.get_filesystem(big_file) == filesystem
