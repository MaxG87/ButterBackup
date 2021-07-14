from __future__ import annotations

from subprocess import CalledProcessError

import pytest

from butter_backup import shell_interface as sh


def test_run_cmd_succeeds() -> None:
    proc = sh.run_cmd(cmd="true")
    assert proc.returncode == 0


def test_run_cmd_fails() -> None:
    with pytest.raises(CalledProcessError):
        sh.run_cmd(cmd="false")


def test_run_cmd_captures_output() -> None:
    message = "Das Pferd frisst keinen Gurkensalat."
    proc = sh.run_cmd(cmd=f"echo '{message}'", capture_output=True)
    assert proc.returncode == 0
    assert proc.stdout.strip().decode("utf-8") == message
