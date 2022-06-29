from __future__ import annotations

import contextlib
import secrets
import string
import subprocess
from collections import defaultdict
from datetime import date
from pathlib import Path
from tempfile import TemporaryDirectory
from uuid import UUID

from loguru import logger

from . import config_parser as cp
from . import shell_interface as sh


class InvalidDecryptedDevice(ValueError):
    pass


@contextlib.contextmanager
def decrypted_device(device: Path, pass_cmd: str):
    decrypted = open_encrypted_device(device, pass_cmd)
    logger.success(f"Gerät {device} erfolgreich entschlüsselt.")
    try:
        yield decrypted
    finally:
        close_decrypted_device(decrypted)
        logger.success(f"Verschlüsselung des Gerätes {device} erfolgreich geschlossen.")


@contextlib.contextmanager
def mounted_device(device: Path):
    if is_mounted(device):
        unmount_device(device)
    with TemporaryDirectory() as td:
        mount_dir = Path(td)
        mount_btrfs_device(device, Path(mount_dir))
        logger.success(f"Gerät {device} erfolgreich nach {mount_dir} gemountet.")
        try:
            yield Path(mount_dir)
        finally:
            unmount_device(device)
            logger.success(f"Gerät {device} erfolgreich ausgehangen.")


@contextlib.contextmanager
def symbolic_link(src: Path, dest: Path):
    """Create an symbolic link from `src` to `dest`

    This context manager will create a symbolic link from src to dest. It
    differentiates itself from `Path.link_to()` by …:

        * … creating the link with root privileges. This allows to limit root
          permissions to only the necessary parts of the program.

        * ensuring that the link gets removed after usage.

    Parameters:
    -----------
    src: Path to source; can be anything that has a filesystem path
    dest: Path to destination file

    Returns:
    --------
    The value of `dest.absolute()` will be returned.
    """

    if not src.exists():
        raise FileNotFoundError
    if dest.exists():
        raise FileExistsError
    absolute_dest = dest.absolute()
    ln_cmd: sh.StrPathList = ["sudo", "ln", "-s", src.absolute(), absolute_dest]
    sh.run_cmd(cmd=ln_cmd)
    logger.success(f"Symlink von {src} nach {dest} erfolgreich erstellt.")
    try:
        yield absolute_dest
    finally:
        # In case the link destination vanished, the program must not crash. After
        # all, the aimed for state has been reached.
        rm_cmd: sh.StrPathList = ["sudo", "rm", "-f", absolute_dest]
        sh.run_cmd(cmd=rm_cmd)
        logger.success(f"Symlink von {src} nach {dest} erfolgreich entfernt.")


def mount_btrfs_device(device: Path, mount_dir: Path) -> None:
    cmd: sh.StrPathList = [
        "sudo",
        "mount",
        "-o",
        "compress=zlib",
        device,
        mount_dir,
    ]
    sh.run_cmd(cmd=cmd)


def is_mounted(dest: Path) -> bool:
    dest_as_str = str(dest)
    try:
        mount_dest = get_mounted_devices()[dest_as_str]
        logger.info(f"Mount des Gerät {dest} in {mount_dest} gefunden.")
    except KeyError:
        logger.info(f"Kein Mountpunkt für Gerät {dest} gefunden.")
        return False
    return True


def get_mounted_devices() -> dict[str, set[Path]]:
    raw_mounts = sh.run_cmd(cmd=["mount"], capture_output=True)
    mount_lines = raw_mounts.stdout.decode().splitlines()
    mount_points = defaultdict(set)
    for line in mount_lines:
        device = line.split()[0]
        dest = Path(line.split()[2])
        mount_points[device].add(dest)
    return dict(mount_points)


def unmount_device(device: Path) -> None:
    cmd: sh.StrPathList = ["sudo", "umount", device]
    sh.run_cmd(cmd=cmd)


def open_encrypted_device(device: Path, pass_cmd: str) -> Path:
    map_name = device.name
    decrypt_cmd: sh.StrPathList = ["sudo", "cryptsetup", "open", device, map_name]
    pwd_proc = subprocess.run(pass_cmd, stdout=subprocess.PIPE, shell=True, check=True)
    subprocess.run(decrypt_cmd, input=pwd_proc.stdout, check=True)
    return Path("/dev/mapper/") / map_name


def close_decrypted_device(device: Path) -> None:
    if device.parent != Path("/dev/mapper"):
        raise InvalidDecryptedDevice
    map_name = device.name
    close_cmd = ["sudo", "cryptsetup", "close", map_name]
    sh.run_cmd(cmd=close_cmd)


def encrypt_device(device: Path, password_cmd: str) -> None:
    format_cmd: sh.StrPathList = ["sudo", "cryptsetup", "luksFormat", device]
    sh.pipe_pass_cmd_to_real_cmd(pass_cmd=password_cmd, command=format_cmd)


def prepare_device_for_butterbackend(uuid: UUID) -> cp.BtrfsConfig:
    password_cmd = generate_passcmd()
    config = cp.BtrfsConfig(
        BackupRepositoryFolder="ButterBackupRepository",
        DevicePassCmd=password_cmd,
        Files=set(),
        FilesDest="Einzeldateien",
        Folders={},
        UUID=uuid,
    )
    device = config.device()
    encrypt_device(device, password_cmd)
    with decrypted_device(device, password_cmd) as decrypted:
        mkfs_btrfs(decrypted)
        with mounted_device(decrypted) as mounted:
            backup_repository = mounted / config.BackupRepositoryFolder
            sh.run_cmd(cmd=["sudo", "mkdir", backup_repository])
            initial_subvol = backup_repository / date.today().strftime(
                cp.BtrfsConfig.SubvolTimestampFmt
            )
            sh.run_cmd(cmd=["sudo", "btrfs", "subvolume", "create", initial_subvol])
    return config


def prepare_device_for_resticbackend(uuid: UUID) -> cp.ResticConfig:
    device_passcmd = generate_passcmd()
    repository_passcmd = generate_passcmd()
    config = cp.ResticConfig(
        BackupRepositoryFolder="ResticBackupRepository",
        DevicePassCmd=device_passcmd,
        FilesAndFolders=set(),
        RepositoryPassCmd=repository_passcmd,
        UUID=uuid,
    )
    device = config.device()
    encrypt_device(device, device_passcmd)
    with decrypted_device(device, device_passcmd) as decrypted:
        mkfs_btrfs(decrypted)
        with mounted_device(decrypted) as mounted:
            backup_repo = mounted / config.BackupRepositoryFolder
            sh.run_cmd(cmd=["sudo", "mkdir", backup_repo])
            sh.pipe_pass_cmd_to_real_cmd(
                repository_passcmd,
                ["sudo", "restic", "init", "-r", backup_repo],
            )
    return config


def mkfs_btrfs(file: Path) -> None:
    cmd: sh.StrPathList = ["sudo", "mkfs.btrfs", file]
    sh.run_cmd(cmd=cmd)


def generate_passcmd() -> str:
    """
    Generate `echo` safe password and return PassCmd

    Returns
    -------
    str
        password command producing the password
    """
    n_chars = 16
    alphabet = string.ascii_letters + string.digits
    passphrase = "".join(secrets.choice(alphabet) for _ in range(n_chars))
    return f"echo {passphrase}"
