import abc
import datetime as dt
from dataclasses import dataclass
from pathlib import Path
from typing import overload

import shell_interface as sh
from loguru import logger

from . import config_parser as cp


def _refresh_sudo(sudo_pass_cmd: str | None) -> None:
    if sudo_pass_cmd is not None:
        sh.pipe_pass_cmd_to_real_cmd(
            sudo_pass_cmd, ["sudo", "-Sv"], capture_output=True
        )


class BackupBackend(abc.ABC):
    @abc.abstractmethod
    def do_backup(self, mount_dir: Path, sudo_pass_cmd: str | None = None) -> None: ...

    @overload
    @staticmethod
    def from_config(config: cp.BtrFSRsyncConfig) -> "BtrFSRsyncBackend": ...

    @overload
    @staticmethod
    def from_config(config: cp.ResticConfig) -> "ResticBackend": ...

    @staticmethod
    def from_config(
        config: cp.BtrFSRsyncConfig | cp.ResticConfig,
    ) -> "BtrFSRsyncBackend | ResticBackend":
        # Getestet durch Tests der Backuplogik
        if isinstance(config, cp.BtrFSRsyncConfig):
            return BtrFSRsyncBackend(config=config)
        return ResticBackend(config=config)


@dataclass(frozen=True)
class BtrFSRsyncBackend(BackupBackend):
    config: cp.BtrFSRsyncConfig

    def do_backup(self, mount_dir: Path, sudo_pass_cmd: str | None = None) -> None:
        logger.info(f"Beginne mit BtrFS-Backup für Speichermedium {self.config.Name}.")
        backup_repository = mount_dir / self.config.BackupRepositoryFolder
        src_snapshot = self.get_source_snapshot(backup_repository)
        logger.info(f"Basis-Sicherungskopie: {src_snapshot}.")
        backup_root = self.snapshot(
            src=src_snapshot, backup_repository=backup_repository
        )
        _refresh_sudo(sudo_pass_cmd)
        self.adapt_ownership(backup_root)

        for src, dest_name in self.config.Folders.items():
            dest = backup_root / dest_name
            _refresh_sudo(sudo_pass_cmd)
            self.rsync_folder(src, dest, self.config.ExcludePatternsFile)

        files_dest = backup_root / self.config.FilesDest
        if files_dest.is_file():
            files_dest.unlink()
        files_dest.mkdir(parents=True, exist_ok=True)
        for src in self.config.Files:
            _refresh_sudo(sudo_pass_cmd)
            self.rsync_file(src, files_dest)

    @staticmethod
    def get_source_snapshot(root: Path) -> Path:
        return max(root.glob("202?-*"))

    @staticmethod
    def adapt_ownership(snapshot_root: Path) -> None:
        user = sh.get_user()
        group = sh.get_group(user)
        logger.debug(
            "Übertrage Besitzrechte von {backup_root} nicht-rekursiv an {user}:{group}.",
            backup_root=snapshot_root,
            user=user,
            group=group,
        )
        # The intention is to only change the ownership of the snapshot root, not its
        # contents. However, since afterwards a backup using `rsync --delete` is
        # performed, it is believed that changing the ownership of the snapshot root
        # recursively is not incorrect neither. After the backup, every pre-existing
        # file and folder will either have been deleted or will have been adapted to the
        # correct ownership.
        # Therefore, it is believed that writing test that fails if `recursive=True` is
        # currently impossible.
        sh.chown(snapshot_root, user, group, recursive=False)

    def snapshot(self, *, src: Path, backup_repository: Path) -> Path:
        timestamp = dt.datetime.now()
        date_fmt = self.config.SubvolTimestampFmt
        while True:
            # In order to get rid of sleeps in the tests, snapshot name
            # collisions must be handled gracefully. Incrementing the timestamp
            # by a few seconds seems to be acceptable both in test and real
            # world scenarios.
            # There is still the possibility of race conditions, where another
            # process would create the target folder before a snapshot can be
            # created. This possibility is believed to be negligible, so
            # nothing is done to prevent it.
            backup_root = backup_repository / timestamp.strftime(date_fmt)
            if backup_root.exists():
                timestamp += dt.timedelta(seconds=1)
            else:
                break
        cmd: sh.StrPathList = [
            "sudo",
            "btrfs",
            "subvolume",
            "snapshot",
            src,
            backup_root,
        ]
        sh.run_cmd(cmd=cmd)
        return backup_root

    @staticmethod
    def rsync_file(src: Path, dest: Path) -> None:
        cmd: sh.StrPathList = ["sudo", "rsync", "-ax", "--inplace", src, dest]
        sh.run_cmd(cmd=cmd)

    @staticmethod
    def rsync_folder(
        src: Path, dest: Path, maybe_exclude_patterns: Path | None
    ) -> None:
        cmd: sh.StrPathList = [
            "sudo",
            "rsync",
            "-ax",
            "--delete",
            "--delete-excluded",
            "--inplace",
        ]
        if maybe_exclude_patterns is not None:
            cmd.extend(["--exclude-from", maybe_exclude_patterns])
        cmd.extend([f"{src}/", dest])
        sh.run_cmd(cmd=cmd)


@dataclass(frozen=True)
class ResticBackend(BackupBackend):
    config: cp.ResticConfig

    def do_backup(self, mount_dir: Path, sudo_pass_cmd: str | None = None) -> None:
        logger.info(f"Beginne mit Restic-Backup für Speichermedium {self.config.Name}.")
        backup_repository = mount_dir / self.config.BackupRepositoryFolder
        _refresh_sudo(sudo_pass_cmd)
        self.copy_files(backup_repository)
        _refresh_sudo(sudo_pass_cmd)
        self.adapt_ownership(backup_repository)

    @staticmethod
    def adapt_ownership(backup_repository: Path) -> None:
        user = sh.get_user()
        group = sh.get_group(user)
        logger.debug(
            "Übertrage Besitzrechte von {backup_root} rekursiv an {user}:{group}.",
            backup_root=backup_repository,
            user=user,
            group=group,
        )
        sh.chown(backup_repository, user, group, recursive=True)

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
