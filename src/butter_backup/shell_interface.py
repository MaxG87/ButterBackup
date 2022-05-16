from __future__ import annotations

import os
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
    capture_output: bool = False,
) -> subprocess.CompletedProcess[bytes]:
    if env is None:
        env = dict(os.environ)
    result = subprocess.run(cmd, capture_output=capture_output, check=True, env=env)
    return result


def pipe_pass_cmd_to_real_cmd(
    pass_cmd: str, command: StrPathList
) -> subprocess.CompletedProcess[bytes]:
    pwd_proc = subprocess.run(pass_cmd, stdout=subprocess.PIPE, shell=True, check=True)
    completed_process = subprocess.run(command, input=pwd_proc.stdout, check=True)
    return completed_process
