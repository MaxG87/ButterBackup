from __future__ import annotations

from subprocess import CalledProcessError

import pytest
from hypothesis import example, given
from hypothesis import strategies as st

from butter_backup import shell_interface as sh

echoable_text = st.text(
    st.text(alphabet=st.characters(blacklist_categories="C"), min_size=1).map(str.strip)
)


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


def test_run_piped_commands_fails_on_empty_list() -> None:
    empty_cmd: list[list[str]] = []
    with pytest.raises(sh.ShellInterfaceError):
        sh.run_piped_commands(cmds=empty_cmd)


def test_run_piped_commands_fails_on_one_element_list() -> None:
    with pytest.raises(sh.ShellInterfaceError):
        sh.run_piped_commands(cmds=[["echo", "Hallo Welt"]])


def test_run_piped_commands_works() -> None:
    commands = [["echo", "Hallo Welt"], ["grep", "-o", "Welt"]]
    proc = sh.run_piped_commands(cmds=commands)
    assert proc.stdout.strip().decode("utf-8") == "Welt"
