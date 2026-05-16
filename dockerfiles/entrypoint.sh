#!/usr/bin/env bash

set -euo pipefail

# Create /dev/disk/by-uuid/. It is required by the test suite but not available
# in Docker containers by default. It also cannot be created during the build
# stage, because /dev will be mounted from the host.
if command -v sudo &>/dev/null; then
    sudo mkdir -p /dev/disk/by-uuid/
else
    mkdir -p /dev/disk/by-uuid/
fi
exec uv run pytest --no-cov --stepwise .
