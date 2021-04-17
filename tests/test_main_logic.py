from pathlib import Path

import pytest

from butter_backup import __main__ as bb


def test_useful_error_on_missing_file_name():
    missing_cfg = Path("/path/to/nowhere/butter-backup.cfg")
    with pytest.raises(SystemExit) as sysexit:
        bb.load_configuration(missing_cfg)
    assert str(missing_cfg) in str(sysexit.value)
    assert "--help" in str(sysexit.value)
