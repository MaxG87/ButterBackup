from pathlib import Path

import pytest

import shell_interface as sh


@pytest.mark.parametrize(
    "start", ["subdir", "sub dir/with spaces/", "very/deeply/nested/subdir", None]
)
@pytest.mark.parametrize(
    "stop", ["stopDir", "stop dir with spaces", "deeply/nested/stopDir"]
)
def test_rmdir_up_to_works(
    root_owned_tmp_path: Path, start: str | None, stop: str
) -> None:
    stop_p = root_owned_tmp_path / stop
    start_p = stop_p if start is None else stop_p / start
    sh.ensure_directory(start_p)
    assert start_p.exists()
    sh.rmdir_up_to(start_p, stop_p)
    assert not start_p.exists()
    assert not stop_p.exists()
    assert stop_p.parent.exists()
