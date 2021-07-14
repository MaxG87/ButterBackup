from __future__ import annotations

import subprocess
from pathlib import Path
from typing import List, Optional, Union

_CMD_LIST = Union[List[str], List[Path], List[Union[str, Path]]]
_LISTS_OF_CMD_LIST = Union[
    List[List[str]], List[List[Path]], List[List[Union[str, Path]]]
]
StrPathList = List[Union[str, Path]]


class ShellInterfaceError(ValueError):
    pass


def run_cmd(
    *,
    cmd: _CMD_LIST,
    env: Optional[dict[str, str]] = None,
    capture_output: bool = False
) -> subprocess.CompletedProcess:
    if env is None:
        env = {}
    result = subprocess.run(cmd, capture_output=capture_output, check=True, env=env)
    return result


def run_piped_commands(
    *,
    cmds: _LISTS_OF_CMD_LIST,
    env: Optional[dict[str, str]] = None,
    capture_output: bool = False
) -> subprocess.CompletedProcess:
    if env is None:
        env = {}
    if len(cmds) < 2:
        raise ShellInterfaceError("Mindestens zwei Shell-Kommandos erwartet!")

    cmd = cmds.pop(0)
    new_proc = subprocess.run(cmd, stdout=subprocess.PIPE, check=True, env=env)
    for cur_cmd in cmds:
        old_proc = new_proc
        new_proc = subprocess.run(
            cur_cmd,
            input=old_proc.stdout,
            stdout=subprocess.PIPE,
            check=True,
            env=env,
        )
    return new_proc
