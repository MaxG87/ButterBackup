from __future__ import annotations

import getpass
import os
import subprocess
from pathlib import Path
from types import SimpleNamespace

try:
    from loguru import logger  # type: ignore[import, unused-ignore]

    logger.disable("shell_interface")
except ModuleNotFoundError:
    logger = SimpleNamespace()  # type: ignore[assignment, unused-ignore]
    logger.debug = lambda msg: None  # type: ignore[assignment, unused-ignore]
    logger.error = lambda msg: None  # type: ignore[assignment, unused-ignore]

StrPathList = list[str | Path]
_CMD_LIST = list[str] | list[Path] | StrPathList


class PassCmdError(RuntimeError):
    pass


class ShellInterfaceError(RuntimeError):
    pass


def run_cmd(
    *,
    cmd: _CMD_LIST,
    env: dict[str, str] | None = None,
    capture_output: bool = False,
) -> subprocess.CompletedProcess[bytes]:
    """
    Run provided command in shell

    This function will run the provided command in a shell. The command must be
    provided as a list of tokens.

    The main difference to using `subprocess.run` directly is that this function
    checks the return code of the command by default, while `subprocess.run`
    does not. If a non-zero return code is encountered, a `ShellInterfaceError`
    is raised.

    Parameters:
    -----------
    cmd
        command to run in shell
    env
        environment variables to set for the command; if `None`, the current
        environment is used
    capture_output
        whether to capture the output of the command; if `True`, the output is
        returned as part of the `CompletedProcess` object

    Returns:
    --------
    subprocess.CompletedProcess[bytes]
        object containing information about the completed process

    Raises:
    -------
    ShellInterfaceError
        if the command returns a non-zero return code
    """
    if env is None:
        env = dict(os.environ)
    logger.debug(f"Shell-Befehl ist `{cmd}`.")
    try:
        result = subprocess.run(cmd, capture_output=capture_output, check=True, env=env)
    except subprocess.CalledProcessError as e:
        errmsg = f"Shell-Befehl `{cmd}` ist fehlgeschlagen."
        logger.error(errmsg)
        raise ShellInterfaceError(errmsg) from e
    return result


def pipe_pass_cmd_to_real_cmd(
    pass_cmd: str, command: _CMD_LIST, *, capture_output: bool = False
) -> subprocess.CompletedProcess[bytes]:
    """
    Pipe result of first command to second command

    This function will run the first command in a shell and pipe its output to
    the second command. The first command must be provided as a string, while
    the second command must be provided as a list of tokens. This allows to take a plain
    command as input from a user without having to deal with tokenization.

    The return code of both commands is checked. If a non-zero return code is
    encountered, a `ShellInterfaceError` is raised.

    Parameters:
    -----------
    pass_cmd
        command to run in shell and whose output is piped to the second command
    command
        command to run in shell and whose input is piped from the first command
    capture_output
        whether to capture the output of the real command; if `True`, the output is
        returned as part of the `CompletedProcess` object

    Returns:
    --------
    subprocess.CompletedProcess[bytes]
        object containing information about the completed process of the second
        command

    Raises:
    -------
    PassCmdError
        if the password command returns a non-zero return code
    ShellInterfaceError
        if the real command returns a non-zero return code
    """
    logger.debug(f"Shell-Befehl ist `{command}`.")
    try:
        pwd_proc = subprocess.run(
            pass_cmd, stdout=subprocess.PIPE, shell=True, check=True
        )
    except subprocess.CalledProcessError as e:
        errmsg = f"Shell-Befehl `{pass_cmd}` ist fehlgeschlagen."
        logger.error(errmsg)
        raise PassCmdError(errmsg) from e
    try:
        completed_process = subprocess.run(
            command, input=pwd_proc.stdout, check=True, capture_output=capture_output
        )
    except subprocess.CalledProcessError as e:
        errmsg = f"Shell-Befehl `{command}` ist fehlgeschlagen."
        logger.error(errmsg)
        raise ShellInterfaceError(errmsg) from e
    return completed_process


def ensure_directory(directory: Path) -> Path | None:
    """Ensure a directory exists, creating it with root privileges if needed.

    Parameters:
    -----------
    directory
        directory that should exist

    Returns:
    --------
    Path | None
        The first missing ancestor that had to be created, or ``None`` if the
        directory already existed
    """
    if directory.is_dir():
        return None
    first_created = next(
        (parent for parent in reversed(directory.parents) if not parent.is_dir()),
        directory,
    )
    cmd: StrPathList = ["sudo", "mkdir", "-p", directory]
    run_cmd(cmd=cmd)
    return first_created


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
