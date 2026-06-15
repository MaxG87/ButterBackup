from __future__ import annotations

import typing as t

import pytest

import storage_device_managers as sdm


@pytest.mark.parametrize(
    "mkfs_func,name", [(sdm.mkfs_btrfs, "btrfs"), (sdm.mkfs_ext4, "ext4")]
)
def test_mkfs_ext4(big_file, mkfs_func, name) -> None:
    mkfs_func(big_file)
    assert sdm.get_filesystem(big_file) == name


@pytest.mark.parametrize("filesystem", t.get_args(sdm.ValidFileSystems))
def test_mkfs(big_file, filesystem) -> None:
    sdm.mkfs(big_file, filesystem)
    assert sdm.get_filesystem(big_file) == filesystem
