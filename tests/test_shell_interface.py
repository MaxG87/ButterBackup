from __future__ import annotations

from subprocess import CalledProcessError

import pytest
from hypothesis import example, given
from hypothesis import strategies as st

from butter_backup import shell_interface as sh

echoable_text = st.text(
    alphabet=st.characters(blacklist_categories="C"), min_size=1
).map(str.strip)


def test_run_cmd_succeeds() -> None:
    proc = sh.run_cmd(cmd=["true"])
    assert proc.returncode == 0


def test_run_cmd_fails() -> None:
    with pytest.raises(CalledProcessError):
        sh.run_cmd(cmd=["false"])


@given(message=echoable_text)
@example(message="Das Pferd frisst keinen Gurkensalat.")
def test_run_cmd_captures_output(message: str) -> None:
    proc = sh.run_cmd(cmd=["echo", message], capture_output=True)
    assert proc.returncode == 0
    assert proc.stdout.strip().decode("utf-8") == message


def test_pipe_pass_cmd_to_cmd() -> None:
    pass_cmd = "echo Hallo Welt"
    real_command: sh.StrPathList = ["grep", "-o", "Welt"]
    proc = sh.pipe_pass_cmd_to_real_cmd(pass_cmd, real_command)
    assert proc.args == real_command
    assert proc.returncode == 0


def test_pipe_pass_cmd_to_cmd_breaks_on_failing_pass_cmd() -> None:
    pass_cmd = "false"
    real_command: sh.StrPathList = ["true"]
    with pytest.raises(CalledProcessError):
        sh.pipe_pass_cmd_to_real_cmd(pass_cmd, real_command)


def test_pipe_pass_cmd_to_cmd_breaks_on_failing_real_cmd() -> None:
    pass_cmd = "echo Hallo Welt"
    real_command: sh.StrPathList = ["false"]
    with pytest.raises(CalledProcessError):
        sh.pipe_pass_cmd_to_real_cmd(pass_cmd, real_command)
