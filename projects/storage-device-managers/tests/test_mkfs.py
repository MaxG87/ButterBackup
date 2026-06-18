from __future__ import annotations

import typing as t

import pytest

import storage_device_managers as sdm


@pytest.mark.parametrize("filesystem", t.get_args(sdm.ValidFileSystems))
def test_mkfs(big_file, filesystem) -> None:
    sdm.mkfs(big_file, filesystem)
    assert sdm.get_filesystem(big_file) == filesystem
