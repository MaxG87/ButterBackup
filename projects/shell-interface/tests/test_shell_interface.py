from __future__ import annotations

import pytest
from hypothesis import example, given
from hypothesis import strategies as st

import shell_interface as sh

environment_variable_names = st.text(alphabet="ABCDEFGHIJKLMNOPQRSTUVWXYZ_", min_size=1)
echoable_text = st.text(
    alphabet=st.characters(blacklist_categories={"C"}), min_size=1
).map(str.strip)


def test_run_cmd_succeeds() -> None:
    proc = sh.run_cmd(cmd=["true"])
    assert proc.returncode == 0


def test_run_cmd_fails() -> None:
    with pytest.raises(sh.ShellInterfaceError):
        sh.run_cmd(cmd=["false"])


@given(environment=st.dictionaries(environment_variable_names, echoable_text))
def test_run_cmd_forwards_env(environment: dict[str, str]) -> None:
    proc = sh.run_cmd(cmd=["env"], capture_output=True, env=environment)
    stdout = proc.stdout.decode()
    assert proc.returncode == 0
    assert all(cur_var in stdout for cur_var in environment)
    assert all(cur_val in stdout for cur_val in environment.values())


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


def test_pipe_pass_cmd_to_cmd_captures_stdout() -> None:
    pass_cmd = "echo Hallo Welt"
    real_command: sh.StrPathList = ["grep", "-o", "Welt"]
    proc = sh.pipe_pass_cmd_to_real_cmd(pass_cmd, real_command, capture_output=True)
    assert proc.args == real_command
    assert proc.stdout.strip().decode("utf-8") == "Welt"


@pytest.mark.parametrize(
    "failing_cmd",
    ["false", "(echo some_password;  false)"],
)
def test_pipe_pass_cmd_to_cmd_breaks_on_failing_pass_cmd(failing_cmd: str) -> None:
    pass_cmd = "false"
    real_command: sh.StrPathList = ["true"]
    with pytest.raises(sh.PassCmdError):
        sh.pipe_pass_cmd_to_real_cmd(pass_cmd, real_command)


def test_pipe_pass_cmd_to_cmd_breaks_on_failing_real_cmd() -> None:
    pass_cmd = "echo Hallo Welt"
    real_command = ["false"]
    with pytest.raises(sh.ShellInterfaceError):
        sh.pipe_pass_cmd_to_real_cmd(pass_cmd, real_command)
