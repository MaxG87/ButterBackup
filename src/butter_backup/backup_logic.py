from __future__ import annotations

import datetime as dt
from pathlib import Path

from butter_backup import config_parser as cp
from butter_backup import device_managers as dm
from butter_backup import shell_interface as sh

DEFAULT_CONFIG_DIR = Path("~/.config/").expanduser()
DEFAULT_CONFIG_NAME = Path("butter-backup.cfg")
DEFAULT_CONFIG = DEFAULT_CONFIG_DIR / DEFAULT_CONFIG_NAME


def do_backup(config: Path) -> None:
    config_list = cp.load_configuration(config)
    for raw_cfg in config_list:
        parsed_cfg = cp.ParsedButterConfig.from_dict(raw_cfg)
        cfg = cp.ButterConfig.from_raw_config(parsed_cfg)
        if cfg.device().exists():
            do_butter_backup(cfg)


def do_butter_backup(cfg: cp.ButterConfig) -> None:
    with dm.decrypted_device(cfg.device(), cfg.pass_cmd) as decrypted:
        with dm.mounted_device(decrypted) as mount_dir:
            backup_root = mount_dir / dt.datetime.now().strftime("%F_%H:%M:%S")
            src_snapshot = get_source_snapshot(mount_dir)

            snapshot(src=src_snapshot, dest=backup_root)
            for src, dest_name in cfg.folders:
                dest = backup_root / dest_name
                rsync_folder(src, dest)

            files_dest = backup_root / cfg.files_dest
            for src in cfg.files:
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
