from __future__ import annotations

import subprocess
from typing import Optional


def run_cmd(
    *, cmd: str, env: Optional[dict[str, str]] = None, capture_output: bool = False
) -> subprocess.CompletedProcess:
    if env is None:
        env = {}
    result = subprocess.run(
        cmd, capture_output=capture_output, check=True, env=env, shell=True
    )
    return result
