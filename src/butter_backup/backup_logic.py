from __future__ import annotations

import datetime as dt
from pathlib import Path

from butter_backup import config_parser as cp
from butter_backup import device_managers as dm
from butter_backup import shell_interface as sh


def do_backup(config: Path) -> None:
    configurations = list(cp.load_configuration(config))
    for cfg in configurations:
        if isinstance(cfg, cp.ResticConfig):
            print(
                "Konfiguration für Restic gefunden. Restic wird noch nicht"
                " unterstützt, daher wird diese Konfiguration übersprungen."
            )
            continue
        if cfg.device().exists():
            do_butter_backup(cfg)


def do_butter_backup(cfg: cp.BtrfsConfig) -> None:
    with dm.decrypted_device(cfg.device(), cfg.PassCmd) as decrypted:
        with dm.mounted_device(decrypted) as mount_dir:
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
