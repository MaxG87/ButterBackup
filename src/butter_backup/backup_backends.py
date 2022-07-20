import abc
import datetime as dt
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from loguru import logger

from . import config_parser as cp
from . import shell_interface as sh


class BackupBackend(abc.ABC):
    @abc.abstractmethod
    def do_backup(self, mount_dir: Path) -> None:
        ...


@dataclass(frozen=True)
class ButterBackend(BackupBackend):
    config: cp.BtrfsConfig

    def do_backup(self, mount_dir: Path) -> None:
        logger.info(f"Beginne mit BtrFS-Backup für Gerät {self.config.UUID}.")
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
        logger.info(f"Beginne mit Restic-Backup für Gerät {self.config.UUID}.")
        restic_cmd: sh.StrPathList = [
            "sudo",
            "restic",
            "backup",
            "--repo",
            mount_dir / self.config.BackupRepositoryFolder,
        ]
        if self.config.ExcludePatternsFile is not None:
            restic_cmd.extend(["--exclude-file", self.config.ExcludePatternsFile])
        restic_cmd.extend(list(self.config.FilesAndFolders))
        sh.pipe_pass_cmd_to_real_cmd(self.config.RepositoryPassCmd, restic_cmd)
