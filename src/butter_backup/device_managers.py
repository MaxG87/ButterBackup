from __future__ import annotations

from datetime import date
from pathlib import Path

import shell_interface as sh
import storage_device_managers as sdm

from . import config_parser as cp


def prepare_device_for_butterbackend(device: Path) -> cp.BtrFSRsyncConfig:
    password_cmd = sdm.generate_passcmd()
    backup_repository_folder = "ButterBackupRepository"
    volume_uuid = sdm.encrypt_device(device, password_cmd)
    compression = sdm.ValidCompressions.ZSTD
    user = sh.get_user()
    group = sh.get_group(user)
    with sdm.decrypted_device(device, password_cmd) as decrypted:
        sdm.mkfs_btrfs(decrypted)
        with sdm.mounted_device(decrypted) as mounted:
            backup_repository = mounted / backup_repository_folder
            mkdir_cmd: sh.StrPathList = ["sudo", "mkdir", backup_repository]
            sh.run_cmd(cmd=mkdir_cmd)

            initial_subvol = backup_repository / date.today().strftime(
                cp.BtrFSRsyncConfig.SubvolTimestampFmt
            )
            subvol_cmd: sh.StrPathList = [
                "sudo",
                "btrfs",
                "subvolume",
                "create",
                initial_subvol,
            ]
            sh.run_cmd(cmd=subvol_cmd)
            sdm.chown(mounted, user, group, recursive=True)

    config = cp.BtrFSRsyncConfig(
        BackupRepositoryFolder=backup_repository_folder,
        Compression=compression,
        DevicePassCmd=password_cmd,
        Files=set(),
        FilesDest="Einzeldateien",
        Folders={},
        UUID=volume_uuid,
    )
    return config


def prepare_device_for_resticbackend(device: Path) -> cp.ResticConfig:
    device_passcmd = sdm.generate_passcmd()
    repository_passcmd = sdm.generate_passcmd()
    backup_repository_folder = "ResticBackupRepository"
    compression = None  # Restic encrypts and encrypted data are incompressible
    volume_uuid = sdm.encrypt_device(device, device_passcmd)
    user = sh.get_user()
    group = sh.get_group(user)
    with sdm.decrypted_device(device, device_passcmd) as decrypted:
        sdm.mkfs_btrfs(decrypted)
        with sdm.mounted_device(decrypted) as mounted:
            backup_repo = mounted / backup_repository_folder
            mkdir_repo: sh.StrPathList = ["sudo", "mkdir", backup_repo]
            restic_init: sh.StrPathList = [
                "sudo",
                "restic",
                "init",
                "-r",
                backup_repo,
            ]
            sh.run_cmd(cmd=mkdir_repo)
            sh.pipe_pass_cmd_to_real_cmd(repository_passcmd, restic_init)
            sdm.chown(mounted, user, group, recursive=True)
    config = cp.ResticConfig(
        BackupRepositoryFolder=backup_repository_folder,
        Compression=compression,
        DevicePassCmd=device_passcmd,
        FilesAndFolders=set(),
        RepositoryPassCmd=repository_passcmd,
        UUID=volume_uuid,
    )
    return config
