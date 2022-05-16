from __future__ import annotations

import datetime as dt
from pathlib import Path
from typing import Union

from butter_backup import config_parser as cp
from butter_backup import device_managers as dm
from butter_backup import shell_interface as sh


def do_backup(config: Union[cp.BtrfsConfig, cp.ResticConfig]) -> None:
    if isinstance(config, cp.ResticConfig):
        print(
            "Konfiguration für Restic gefunden. Restic wird noch nicht"
            " unterstützt, daher wird diese Konfiguration übersprungen."
        )
        return
    if config.device().exists():
        with dm.decrypted_device(config.device(), config.DevicePassCmd) as decrypted:
            with dm.mounted_device(decrypted) as mount_dir:
                do_butter_backup(config, mount_dir)


def do_butter_backup(cfg: cp.BtrfsConfig, mount_dir: Path) -> None:
    backup_root = mount_dir / dt.datetime.now().strftime("%F_%H:%M:%S")
    src_snapshot = get_source_snapshot(mount_dir)

    snapshot(src=src_snapshot, dest=backup_root)
    for src, dest_name in cfg.Folders.items():
        dest = backup_root / dest_name
        rsync_folder(src, dest)

    files_dest = backup_root / cfg.FilesDest
    for src in cfg.Files:
        rsync_file(src, files_dest)


def get_source_snapshot(root: Path) -> Path:
    return max(root.glob("202?-*"))


def snapshot(*, src: Path, dest: Path) -> None:
    cmd: sh.StrPathList = ["sudo", "btrfs", "subvolume", "snapshot", src, dest]
    sh.run_cmd(cmd=cmd)


def rsync_file(src: Path, dest: Path) -> None:
    cmd: sh.StrPathList = ["sudo", "rsync", "-ax", "--inplace", src, dest]
    sh.run_cmd(cmd=cmd)


def rsync_folder(src: Path, dest: Path) -> None:
    cmd: sh.StrPathList = [
        "sudo",
        "rsync",
        "-ax",
        "--delete",
        "--inplace",
        f"{src}/",
        dest,
    ]
    sh.run_cmd(cmd=cmd)
