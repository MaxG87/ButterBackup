import typing as t
from collections import defaultdict
from pathlib import Path

import msgspec
import shell_interface as sh

MountOptions = frozenset[str]


class _LoSetupDevice(msgspec.Struct):
    name: str
    back_file: str = msgspec.field(name="back-file")


class _LoSetupOutput(msgspec.Struct):
    loopdevices: list[_LoSetupDevice]


class _FindmntFilesystem(msgspec.Struct):
    source: str
    target: str
    options: str


class _FindmntOutput(msgspec.Struct):
    filesystems: list[_FindmntFilesystem]


def _get_loop_device_backing_files() -> dict[str, str]:
    """Return a mapping from loop device path to its backing file path.

    Example return value:
    {"/dev/loop3": "/tmp/tmpXXX", ...}

    Returns an empty mapping if ``losetup`` is unavailable or reports no devices.
    """
    raw = sh.run_cmd(cmd=["losetup", "--list", "--json"], capture_output=True)
    parsed = msgspec.json.decode(raw.stdout, type=_LoSetupOutput)
    return {dev.name: dev.back_file for dev in parsed.loopdevices}


def get_mounted_devices() -> t.Mapping[str, t.Mapping[Path, MountOptions]]:
    """Get all mounted devices

    This function will parse the output of `findmnt -l --json` and return everything
    that is mounted to somewhere. The returned mapping maps device names (i.e. mount
    sources) to their destinations and mount options.

    Since a source can be mounted to multiple (e.g. /dev/sda1 can be mounted to
    /home/{user1,user2}/Videos), the value of the mapping is another mapping. This inner
    mapping maps mount destinations to their mount options.

    Returns:
    --------
    t.Mapping[str, t.Mapping[Path, MountOptions]]
        A mapping that maps mount sources (i.e. device names) to their
        destinations and mount options.

    Example Return Value:
    ---------------------
    {
        "/dev/nvme0n1p2": {
            Path("/boot"): frozenset({"rw", "relatime"}),
            Path("/media/backup"): frozenset({"rw", "relatime", "compress=zstd:3"}),
        },
    }
    """
    raw = sh.run_cmd(
        cmd=["findmnt", "--list", "--json", "--nofsroot"], capture_output=True
    )
    parsed = msgspec.json.decode(raw.stdout, type=_FindmntOutput)
    loop_backing_mapping = _get_loop_device_backing_files()
    mount_points: dict[str, dict[Path, MountOptions]] = defaultdict(dict)
    for fs in parsed.filesystems:
        source = loop_backing_mapping.get(fs.source, fs.source)
        dest = Path(fs.target)
        options: MountOptions = frozenset(fs.options.split(","))
        mount_points[source][dest] = options
    return dict(mount_points)
