from pathlib import Path
from tempfile import TemporaryDirectory

import pytest

from butter_backup import __main__ as bb


def test_useful_error_on_missing_file_name() -> None:
    missing_cfg = Path("/path/to/nowhere/butter-backup.cfg")
    with pytest.raises(SystemExit) as sysexit:
        bb.load_configuration(missing_cfg)
    assert str(missing_cfg) in str(sysexit.value)
    assert "--help" in str(sysexit.value)


@pytest.mark.parametrize("dest", bb.get_mounted_devices())
def test_is_mounted_detects(dest: Path) -> None:
    assert bb.is_mounted(dest)


def test_is_mounted_rejects() -> None:
    with TemporaryDirectory() as tempd:
        assert not bb.is_mounted(Path(tempd))
