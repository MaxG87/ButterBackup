from __future__ import annotations

import datetime as dt
from pathlib import Path
from typing import Optional, Union

from . import config_parser as cp
from . import device_managers as dm
from . import shell_interface as sh


def do_backup(config: Union[cp.BtrfsConfig, cp.ResticConfig]) -> None:
    if config.device().exists():
        with dm.decrypted_device(config.device(), config.DevicePassCmd) as decrypted:
            with dm.mounted_device(decrypted) as mount_dir:
                if isinstance(config, cp.BtrfsConfig):
                    do_butter_backup(config, mount_dir)
                elif isinstance(config, cp.ResticConfig):
                    do_restic_backup(config, mount_dir)
                else:
                    # Should be unreachable!
                    btrfs_name = str(cp.BtrfsConfig)  # type: ignore[unreachable]
                    restic_name = str(cp.ResticConfig)
                    raise ValueError(
                        f"Nur {btrfs_name} und {restic_name} erlaubt, aber {type(config)} erhalten!"
                    )


def do_butter_backup(cfg: cp.BtrfsConfig, mount_dir: Path) -> None:
    backup_repository = mount_dir / cfg.BackupRepositoryFolder
    backup_root = backup_repository / dt.datetime.now().strftime(cfg.SubvolTimestampFmt)
    src_snapshot = get_source_snapshot(backup_repository)

    snapshot(src=src_snapshot, dest=backup_root)
    for src, dest_name in cfg.Folders.items():
        dest = backup_root / dest_name
        rsync_folder(src, dest, cfg.ExcludePatternsFile)

    files_dest = backup_root / cfg.FilesDest
    for src in cfg.Files:
        rsync_file(src, files_dest)


def do_restic_backup(cfg: cp.ResticConfig, mount_dir: Path) -> None:
    restic_cmd: sh.StrPathList = [
        "sudo",
        "restic",
        "backup",
        "--repo",
        mount_dir / cfg.BackupRepositoryFolder,
    ]
    if cfg.ExcludePatternsFile is not None:
        restic_cmd.extend(["--exclude-file", cfg.ExcludePatternsFile])
    restic_cmd.extend(list(cfg.FilesAndFolders))
    sh.pipe_pass_cmd_to_real_cmd(cfg.RepositoryPassCmd, restic_cmd)


def get_source_snapshot(root: Path) -> Path:
    return max(root.glob("202?-*"))


def snapshot(*, src: Path, dest: Path) -> None:
    cmd: sh.StrPathList = ["sudo", "btrfs", "subvolume", "snapshot", src, dest]
    sh.run_cmd(cmd=cmd)


def rsync_file(src: Path, dest: Path) -> None:
    cmd: sh.StrPathList = ["sudo", "rsync", "-ax", "--inplace", src, dest]
    sh.run_cmd(cmd=cmd)


def rsync_folder(src: Path, dest: Path, maybe_exclude_patterns: Optional[Path]) -> None:
    cmd: sh.StrPathList = [
        "sudo",
        "rsync",
        "-ax",
        "--delete",
        "--inplace",
    ]
    if maybe_exclude_patterns is not None:
        cmd.extend(["--exclude-from", maybe_exclude_patterns])
    cmd.extend([f"{src}/", dest])
    sh.run_cmd(cmd=cmd)
