import contextlib
import subprocess
import typing as t
from datetime import date
from pathlib import Path

import shell_interface as sh
import storage_device_managers as sdm

from . import config_parser as cp


class DevicePassCmdError(RuntimeError):
    """Raised when DevicePassCmd fails to execute successfully."""


def open_encrypted_device(device: Path, pass_cmd: str) -> Path:
    """Open an encrypted device, distinguishing pass_cmd failures from decryption failures.

    Unlike sdm.open_encrypted_device, this function raises DevicePassCmdError
    when the password command itself fails, and DeviceDecryptionError only when
    the actual decryption (cryptsetup) fails.
    """
    map_name = device.name
    decrypt_cmd = ["sudo", "cryptsetup", "open", str(device), map_name]
    try:
        pwd_proc = subprocess.run(
            pass_cmd, stdout=subprocess.PIPE, shell=True, check=True
        )
    except subprocess.CalledProcessError as e:
        raise DevicePassCmdError(
            f"DevicePassCmd '{pass_cmd}' failed to execute."
        ) from e
    try:
        subprocess.run(decrypt_cmd, input=pwd_proc.stdout, check=True)
    except subprocess.CalledProcessError as e:
        raise sdm.DeviceDecryptionError() from e
    return Path("/dev/mapper/") / map_name


@contextlib.contextmanager
def decrypted_device(device: Path, pass_cmd: str) -> t.Iterator[Path]:
    """Context manager that opens and closes an encrypted device.

    Like sdm.decrypted_device, but distinguishes DevicePassCmdError from
    DeviceDecryptionError.
    """
    decrypted = open_encrypted_device(device, pass_cmd)
    try:
        yield decrypted
    finally:
        sdm.close_decrypted_device(decrypted)


ValidFileSystems = t.Literal["ext4", "btrfs"]


def format_device(device: Path, file_system: ValidFileSystems) -> None:
    match file_system:
        case "ext4":
            mkfs_ext4(device)
        case "btrfs":
            sdm.mkfs_btrfs(device)
        case _:
            # TODO: Use t.assert_never when Python 3.11 is the minimum version!
            raise ValueError(
                f"Unsupported file system {file_system} for Restic backend."
            )


def mkfs_ext4(device: Path) -> None:
    mkfs_cmd: sh.StrPathList = ["sudo", "mkfs.ext4", "-F", device]
    sh.run_cmd(cmd=mkfs_cmd)


def prepare_device_for_butterbackend(device: Path) -> cp.BtrFSRsyncConfig:
    password_cmd = sdm.generate_passcmd()
    backup_repository_folder = "ButterBackupRepository"
    volume_uuid = sdm.encrypt_device(device, password_cmd)
    compression = sdm.ValidCompressions.ZSTD
    user = sh.get_user()
    group = sh.get_group(user)
    with sdm.decrypted_device(device, password_cmd) as decrypted:
        format_device(decrypted, "btrfs")
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
        Name="BTRFS-RSYNC-Backup",
        UUID=volume_uuid,
    )
    return config


def prepare_device_for_resticbackend(
    device: Path, file_system: ValidFileSystems
) -> cp.ResticConfig:
    device_passcmd = sdm.generate_passcmd()
    repository_passcmd = sdm.generate_passcmd()
    backup_repository_folder = "ResticBackupRepository"
    volume_uuid = sdm.encrypt_device(device, device_passcmd)
    user = sh.get_user()
    group = sh.get_group(user)
    with sdm.decrypted_device(device, device_passcmd) as decrypted:
        format_device(decrypted, file_system)
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
        DevicePassCmd=device_passcmd,
        FilesAndFolders=set(),
        Name="Restic-Backup",
        RepositoryPassCmd=repository_passcmd,
        UUID=volume_uuid,
    )
    return config
