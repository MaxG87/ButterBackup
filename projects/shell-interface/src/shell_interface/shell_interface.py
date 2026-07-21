from __future__ import annotations

import contextlib
import getpass
from pathlib import Path

from .run_cmd import ShellInterfaceError, pipe_pass_cmd_to_real_cmd, run_cmd

StrPathList = list[str | Path]


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


def chown(
    file_or_folder: Path,
    /,
    user: int | str,
    group: int | str | None = None,
    *,
    recursive: bool,
) -> None:
    """Change user and group of a device or folder

    This function will change the ownership as specified. It requires root
    privileges and will ask for them if not available. If no group is given,
    only the owner is changed.

    If recursive is true, ownership information of all files and folders
    contained by `file_or_folder` will be adapted.

    If `file_or_folder` points to a file, `recursive` must be `False`.
    Otherwise a ValueError will be raised.


    Parameters:
    -----------
    user
        user ID, either as name or as UID
    group
        group ID, either as name or as GID
    recursive
        whether or not to change ownership for content

    Raises:
    --------
    ValueError
        if `file_or_folder` is a file but `recursive` is `True`
    """
    if file_or_folder.is_file() and recursive:
        raise ValueError(
            "First argument must point to a directory if `recursive` is `True`!"
        )

    user_spec = str(user) if group is None else f"{user}:{group}"
    chown_cmd: StrPathList = ["sudo", "chown", user_spec, file_or_folder]
    if recursive:
        chown_cmd.append("--recursive")
    run_cmd(cmd=chown_cmd)


def refresh_sudo(sudo_pass_cmd: str | None) -> None:
    """
    Refresh sudo credentials, if a password command is providedo

    Some use cases, most notably butter-backup and the library storage-device-managers,
    require elevated privileges to run. Passing a password command for each and every
    command that requires elevated privileges is cumbersome and error-prone.

    A trade-off is to refresh the sudo cache once after long-running operations, so that
    the user does not have to enter their password multiple times.

    This function will run the provided password command and pipe its output to the
    `sudo -Sv` command, which refreshes the sudo cache. If no password command is
    provided, this function does nothing. Without a password command, the user will be
    prompted for their password once needed.
    """
    if sudo_pass_cmd is not None:
        pipe_pass_cmd_to_real_cmd(sudo_pass_cmd, ["sudo", "-Sv"], capture_output=True)


def rmdir_up_to(start: Path, stop: Path) -> None:
    """
    Remove all directories from `start` to `stop` (inclusive).

    The directories are removed in a bottom-up manner. Execution stops at the first
    non-empty directory or directly after removing stop, whatever comes first.
    """
    if not start.is_relative_to(stop):
        raise ValueError(f"Start path {start} is not a subpath of stop path {stop}.")
    current = start
    while current.is_relative_to(stop):
        with contextlib.suppress(ShellInterfaceError):
            cmd: StrPathList = ["sudo", "rmdir", current]
            run_cmd(cmd=cmd)
        current = current.parent
