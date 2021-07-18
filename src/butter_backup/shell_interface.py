from __future__ import annotations

import subprocess
from pathlib import Path
from typing import List, Optional, Union

_CMD_LIST = Union[List[str], List[Path], List[Union[str, Path]]]
StrPathList = List[Union[str, Path]]


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
