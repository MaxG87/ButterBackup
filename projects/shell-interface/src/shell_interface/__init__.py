from importlib import metadata

from .shell_interface import (
    PassCmdError,
    ShellInterfaceError,
    StrPathList,
    ensure_directory,
    get_group,
    get_user,
    pipe_pass_cmd_to_real_cmd,
    run_cmd,
)

__version__ = metadata.version(__name__)
__all__ = [
    "PassCmdError",
    "ShellInterfaceError",
    "StrPathList",
    "ensure_directory",
    "get_group",
    "get_user",
    "pipe_pass_cmd_to_real_cmd",
    "run_cmd",
]
