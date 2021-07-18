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
