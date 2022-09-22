from __future__ import annotations

import getpass
import os
import subprocess
from pathlib import Path
from typing import List, Optional, Union

from loguru import logger

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
    logger.debug(f"Shell-Befehl ist `{cmd}`.")
    result = subprocess.run(cmd, capture_output=capture_output, check=True, env=env)
    return result


def pipe_pass_cmd_to_real_cmd(
    pass_cmd: str, command: StrPathList
) -> subprocess.CompletedProcess[bytes]:
    logger.debug(f"Shell-Befehl ist `{command}`.")
    pwd_proc = subprocess.run(pass_cmd, stdout=subprocess.PIPE, shell=True, check=True)
    completed_process = subprocess.run(command, input=pwd_proc.stdout, check=True)
    return completed_process


def get_user() -> str:
    """Get user who started ButterBackup

    This function will determine the user who is running ButterBackup.

    Returns:
    --------
    str
        user name of user who started ButterBackup
    """
    return getpass.getuser()


def get_group(user: str) -> str:
    """Get group of a given user

    This function will determine the "effective" group of the specified user.
    For this it relies on the `id` program from GNU coreutils.

    Returns:
    --------
    str
        name of the group of the specified user
    """
    raw_group = run_cmd(cmd=["id", "-gn", user], capture_output=True)
    group = raw_group.stdout.decode().splitlines()[0]
    return group
