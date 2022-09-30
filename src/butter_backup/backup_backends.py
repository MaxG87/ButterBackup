from __future__ import annotations

import abc
import datetime as dt
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, Union, overload

from loguru import logger

from . import config_parser as cp
from . import shell_interface as sh


class BackupBackend(abc.ABC):
    @abc.abstractmethod
    def do_backup(self, mount_dir: Path) -> None:
        ...

    @overload
    @staticmethod
    def from_config(config: cp.BtrFSRsyncConfig) -> BtrFSRsyncBackend:
        ...

    @overload
    @staticmethod
    def from_config(config: cp.ResticConfig) -> ResticBackend:
        ...

    @staticmethod
    def from_config(
        config: Union[cp.BtrFSRsyncConfig, cp.ResticConfig]
    ) -> Union[BtrFSRsyncBackend, ResticBackend]:
        # Getestet durch Tests der Backuplogik
        if isinstance(config, cp.BtrFSRsyncConfig):
            return BtrFSRsyncBackend(config=config)
        return ResticBackend(config=config)


@dataclass(frozen=True)
class BtrFSRsyncBackend(BackupBackend):
    config: cp.BtrFSRsyncConfig

    def do_backup(self, mount_dir: Path) -> None:
        logger.info(f"Beginne mit BtrFS-Backup für Speichermedium {self.config.UUID}.")
        backup_repository = mount_dir / self.config.BackupRepositoryFolder
        backup_root = backup_repository / dt.datetime.now().strftime(
            self.config.SubvolTimestampFmt
        )
        src_snapshot = self.get_source_snapshot(backup_repository)

        self.snapshot(src=src_snapshot, dest=backup_root)
        for src, dest_name in self.config.Folders.items():
            dest = backup_root / dest_name
            self.rsync_folder(src, dest, self.config.ExcludePatternsFile)

        files_dest = backup_root / self.config.FilesDest
        for src in self.config.Files:
            self.rsync_file(src, files_dest)

    @staticmethod
    def get_source_snapshot(root: Path) -> Path:
        return max(root.glob("202?-*"))

    @staticmethod
    def snapshot(*, src: Path, dest: Path) -> None:
        cmd: sh.StrPathList = ["sudo", "btrfs", "subvolume", "snapshot", src, dest]
        sh.run_cmd(cmd=cmd)

    @staticmethod
    def rsync_file(src: Path, dest: Path) -> None:
        cmd: sh.StrPathList = ["sudo", "rsync", "-ax", "--inplace", src, dest]
        sh.run_cmd(cmd=cmd)

    @staticmethod
    def rsync_folder(
        src: Path, dest: Path, maybe_exclude_patterns: Optional[Path]
    ) -> None:
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


@dataclass(frozen=True)
class ResticBackend(BackupBackend):
    config: cp.ResticConfig

    def do_backup(self, mount_dir: Path) -> None:
        logger.info(f"Beginne mit Restic-Backup für Speichermedium {self.config.UUID}.")
        backup_repository = mount_dir / self.config.BackupRepositoryFolder
        self.copy_files(backup_repository)
        self.adapt_ownership(backup_repository)

    @staticmethod
    def adapt_ownership(backup_repository: Path) -> None:
        user = sh.get_user()
        group = sh.get_group(user)
        chown_cmd: sh.StrPathList = [
            "sudo",
            "chown",
            "-R",
            f"{user}:{group}",
            backup_repository,
        ]
        sh.run_cmd(cmd=chown_cmd)

    def copy_files(self, backup_repository: Path) -> None:
        restic_cmd: sh.StrPathList = [
            "sudo",
            "restic",
            "backup",
            "--one-file-system",
            "--repo",
            backup_repository,
        ]
        if self.config.ExcludePatternsFile is not None:
            restic_cmd.extend(["--exclude-file", self.config.ExcludePatternsFile])
        restic_cmd.extend(list(self.config.FilesAndFolders))
        sh.pipe_pass_cmd_to_real_cmd(self.config.RepositoryPassCmd, restic_cmd)
