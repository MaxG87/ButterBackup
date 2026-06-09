import contextlib
import enum
import secrets
import string
import tempfile
import typing as t
from collections import defaultdict
from collections.abc import Iterator
from importlib import metadata
from pathlib import Path
from types import SimpleNamespace
from uuid import UUID, uuid4

import shell_interface as sh

try:
    from loguru import logger  # type: ignore[import, unused-ignore]

    logger.disable("storage_device_managers")
except ModuleNotFoundError:
    logger = SimpleNamespace()  # type: ignore[assignment, unused-ignore]
    logger.success = lambda msg: None  # type: ignore[assignment, unused-ignore]
    logger.info = lambda msg: None  # type: ignore[assignment, unused-ignore]

__version__ = metadata.version(__name__)

__all__ = [
    "DeviceDecryptionError",
    "InvalidDecryptedDevice",
    "MountOptions",
    "UnmountError",
    "ValidCompressions",
    "ValidFileSystems",
    "chown",
    "close_decrypted_device",
    "decrypted_device",
    "encrypt_device",
    "generate_passcmd",
    "get_filesystem",
    "get_mounted_devices",
    "is_mounted",
    "mkfs",
    "mkfs_btrfs",
    "mkfs_ext4",
    "mount_btrfs_device",
    "mount_device",
    "mount_ext4_device",
    "mounted_device",
    "open_encrypted_device",
    "symbolic_link",
    "sync_device",
    "temporary_directory",
    "unmount_device",
]

MountOptions = frozenset[str]
ValidFileSystems = t.Literal["btrfs", "ext4"]


class DeviceDecryptionError(RuntimeError):
    pass


class InvalidDecryptedDevice(ValueError):
    pass


class UnmountError(RuntimeError):
    pass


class ValidCompressions(enum.StrEnum):
    LZO = "lzo"
    ZLIB = "zlib"
    ZLIB1 = "zlib:1"
    ZLIB2 = "zlib:2"
    ZLIB3 = "zlib:3"
    ZLIB4 = "zlib:4"
    ZLIB5 = "zlib:5"
    ZLIB6 = "zlib:6"
    ZLIB7 = "zlib:7"
    ZLIB8 = "zlib:8"
    ZLIB9 = "zlib:9"
    ZSTD = "zstd"
    ZSTD1 = "zstd:1"
    ZSTD2 = "zstd:2"
    ZSTD3 = "zstd:3"
    ZSTD4 = "zstd:4"
    ZSTD5 = "zstd:5"
    ZSTD6 = "zstd:6"
    ZSTD7 = "zstd:7"
    ZSTD8 = "zstd:8"
    ZSTD9 = "zstd:9"
    ZSTD10 = "zstd:10"
    ZSTD11 = "zstd:11"
    ZSTD12 = "zstd:12"
    ZSTD13 = "zstd:13"
    ZSTD14 = "zstd:14"
    ZSTD15 = "zstd:15"


@contextlib.contextmanager
def temporary_directory() -> Iterator[Path]:
    """Create a temporary directory

    This context manager will create a temporary directory and return its path.
    Upon exit, the directory is removed again.

    Returns:
    --------
    Path
        path to the created temporary directory
    """
    tmpdir = Path(tempfile.mkdtemp())
    try:
        yield tmpdir
    except UnmountError:
        # In case unmounting fails, the mount directory cannot be removed, because it is
        # "busy". There is no use in trying to do `mount_dir.rmdir()`.
        raise
    except Exception:
        tmpdir.rmdir()
        raise
    # Try to remove the mount directory. Path.rmdir() will only succeed if the directory
    # is empty, so if anything happened, mount_dir's contents will not be silently
    # deleted. Previously, shutil.rmtree was used implicitly via TemporaryDirectory,
    # which caused the loss of two terabytes of backup data when unmounting failed.
    tmpdir.rmdir()


@contextlib.contextmanager
def decrypted_device(device: Path, pass_cmd: str) -> Iterator[Path]:
    """Decrypt a given device using pass_cmd

    Given a device and a shell command that outputs a password on STDOUT, this
    context manager will open the device using `cryptsetup`. Upon exit, the
    device is closed again.


    Note that pass_cmd will directly be executed in a subshell. Therefore, DO NOT
    USE UNTRUSTED `pass_cmd`!

    Parameters:
    -----------
    device
        file-like object to be opened with `cryptsetup`
    pass_cmd
        command that prints the device's password on STDOUT

    Returns:
    --------
    Path
        destination of opened device

    Raises:
    -------
    shell_interface.PassCmdError
        if the password command returns a non-zero exit code
    DeviceDecryptionError
        if cryptsetup returns a non-zero exit code
    """
    decrypted = open_encrypted_device(device, pass_cmd)
    logger.success(f"Speichermedium {device} erfolgreich entschlüsselt.")
    try:
        yield decrypted
    finally:
        close_decrypted_device(decrypted)
        logger.success(
            f"Verschlüsselung des Speichermediums {device} erfolgreich geschlossen."
        )


@contextlib.contextmanager
def mounted_device(
    device: Path, compression: ValidCompressions | None = None
) -> Iterator[Path]:
    """Mount a given BtrFS device

    Given a path pointing to a file-like object, this context manager will
    mount it to some temporary directory and return its path. Upon exit, the
    file-like object is unmounted again.

    If `compression` is provided, a mount option specifying the transparent
    file system compression is set. Compression is only supported for BtrFS
    devices. If `compression` is given for a non-BtrFS device, it is silently
    ignored.

    Parameters:
    -----------
    device
        file-like object to be mounted
    compression
        compression level to be used by BtrFS

    Returns:
    --------
    Path
        directory to which `device` was mounted
    """
    if is_mounted(device):
        unmount_device(device)
    with temporary_directory() as mount_dir:
        mount_device(device, mount_dir, compression)
        logger.success(
            f"Speichermedium {device} erfolgreich nach {mount_dir} gemountet."
        )
        try:
            yield Path(mount_dir)
        finally:
            unmount_device(device)
            logger.success(
                "Speichermedium {device} erfolgreich ausgehangen.", device=device
            )


@contextlib.contextmanager
def symbolic_link(src: Path, dest: Path) -> Iterator[Path]:
    """Create a symbolic link from `src` to `dest`

    This context manager will create a symbolic link from src to dest. It
    differentiates itself from `Path.link_to()` by …:

        * … creating the link with root privileges. This allows to limit root
          permissions to only the necessary parts of the program.

        * … ensuring that the link gets removed after usage.

    Parameters:
    -----------
    src: Path to source; can be anything that has a filesystem path
    dest: Path to destination file

    Returns:
    --------
    Path
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


def mount_btrfs_device(
    device: Path, mount_dir: Path, compression: ValidCompressions | None = None
) -> None:
    """
    Mount a given BtrFS device

    Given a path pointing to a file-like object and a target directory, this function
    will mount the device to the target directory.

    The filesystem of `device` must be BtrFS. While technically other file systems
    might work too, this behaviour is not guaranteed and might be broken without
    further notice!

    If `compression` is provided, a mount option specifying the transparent file
    system compression is set.

    Parameters:
    -----------
    device
        file-like object to be mounted
    mount_dir
        directory to which `device` is mounted
    compression
        compression level to be used by BtrFS
    """
    cmd: sh.StrPathList = ["sudo", "mount", device, mount_dir]
    if compression is not None:
        cmd.extend(["-o", f"compress={compression}"])
    sh.run_cmd(cmd=cmd)


def mount_ext4_device(device: Path, mount_dir: Path) -> None:
    """
    Mount a given ext4 device

    Given a path pointing to a file-like object and a target directory, this function
    will mount the device to the target directory.

    The filesystem of `device` must be ext4. While technically other file systems
    might work too, this behaviour is not guaranteed and might be broken without
    further notice!

    Parameters:
    -----------
    device
        file-like object to be mounted
    mount_dir
        directory to which `device` is mounted
    """
    cmd: sh.StrPathList = ["sudo", "mount", "-t", "ext4", device, mount_dir]
    sh.run_cmd(cmd=cmd)


def mount_device(
    device: Path, mount_dir: Path, compression: ValidCompressions | None = None
) -> None:
    """Mount a device without knowing its file system type

    Given a path pointing to a file-like object and a target directory, this
    function will detect the file system of the device and mount it to the
    target directory using the appropriate mount function.

    If `compression` is provided and the file system of `device` is BtrFS, a mount
    option specifying the transparent file system compression is set. For other file
    systems, `compression` is silently ignored.

    Parameters:
    -----------
    device
        file-like object to be mounted
    mount_dir
        directory to which `device` is mounted
    compression
        compression level to be used by BtrFS
    """
    fs = get_filesystem(device)
    match fs:
        case "btrfs":
            mount_btrfs_device(device, mount_dir, compression)
        case "ext4":
            mount_ext4_device(device, mount_dir)
        case _:
            cmd: sh.StrPathList = ["sudo", "mount", device, mount_dir]
            sh.run_cmd(cmd=cmd)


def is_mounted(device: Path) -> bool:
    """Check whether a given device is mounted

    Parameters:
    -----------
    device
        file-like object to be checked

    Returns:
    --------
    bool
        True if `device` is mounted, False otherwise
    """
    device_as_str = str(device)
    try:
        mount_dest = get_mounted_devices()[device_as_str]
        logger.info(f"Mount des Speichermediums {device} in {mount_dest} gefunden.")
    except KeyError:
        logger.info(f"Kein Mountpunkt für Speichermedium {device} gefunden.")
        return False
    return True


def get_mounted_devices() -> t.Mapping[str, t.Mapping[Path, MountOptions]]:
    """Get all mounted devices

    This function will parse the output of `mount` and return everything that is mounted
    to somewhere. The returned mapping maps device names (i.e. mount sources) to their
    destinations and mount options.

    Since a source can be mounted to multiple (e.g. /dev/sda1 can be mounted to
    /home/{user1,user2}/Videos), the value of the mapping is another mapping. This inner
    mapping maps mount destinations to their mount options.

    Returns:
    --------
    t.Mapping[str, t.Mapping[Path, MountOptions]]
        A mapping that maps mount sources (i.e. device names) to their
        destinations and mount options.

    Example Return Value:
    ---------------------
    {
        "/dev/nvme0n1p2": {
            Path("/boot"): frozenset({"rw", "relatime"}),
            Path("/media/backup"): frozenset({"rw", "relatime", "compress=zstd:3"}),
        },
    }
    """
    # Example line:
    # /dev/nvme0n1p2 on /boot type ext2 (rw,relatime)
    raw_mounts = sh.run_cmd(cmd=["mount"], capture_output=True)
    mount_lines = raw_mounts.stdout.decode().splitlines()
    mount_points: dict[str, dict[Path, MountOptions]] = defaultdict(dict)
    for line in mount_lines:
        device = line.split()[0]
        dest = Path(line.split()[2])
        raw_options = line.split()[5]
        options = frozenset(raw_options.strip("()").split(","))
        mount_points[device][dest] = options
    return dict(mount_points)


def sync_device(device: Path) -> None:
    """Sync a device's filesystem

    This function flushes pending writes to the given device. For BtrFS
    devices, it additionally performs a BtrFS-specific filesystem sync on each
    of the device's mount points.

    Parameters:
    -----------
    device
        The device to be synced.
    """
    sync_cmd: sh.StrPathList = ["sudo", "sync", "-f", device]

    try:
        fs = get_filesystem(device)
    except sh.ShellInterfaceError:
        fs = None
    if fs == "btrfs":
        mounted = get_mounted_devices()
        mount_points = mounted.get(str(device), {})
        for mount_dir in mount_points:
            btrfs_sync_cmd: sh.StrPathList = [
                "sudo",
                "btrfs",
                "filesystem",
                "sync",
                mount_dir,
            ]
            sh.run_cmd(cmd=btrfs_sync_cmd)
    sh.run_cmd(cmd=sync_cmd)


def unmount_device(device: Path) -> None:
    """Unmount a given device

    This function will unmount a given device. It relies on the system's
    `umount` program to do so. Before unmounting, the device's filesystem is
    synced to flush any pending writes.

    Parameters:
    -----------
    device
        The device to be unmounted.

    Raises:
    -------
    UnmountError
        if `umount` returns a non-zero exit code
    """
    sync_device(device)
    cmd: sh.StrPathList = ["sudo", "umount", device]
    try:
        sh.run_cmd(cmd=cmd)
    except sh.ShellInterfaceError as e:
        raise UnmountError from e


def open_encrypted_device(device: Path, pass_cmd: str) -> Path:
    """Open an encrypted device

    This function will open an encrypted device. The given path must point to a
    device that can be opened by `cryptsetup`.

    In order to encrypt the device, `pass_cmd` is executed and its output is
    piped into `cryptsetup`. This allows to use any program that can output
    the password to decrypt the device.

    Note that pass_cmd will directly be executed in a subshell. Therefore, DO NOT
    USE UNTRUSTED `pass_cmd`!

    Parameters:
    -----------
    device
        The device to be opened.
    pass_cmd
        The command that outputs the password to decrypt the device.

    Raises:
    -------
    shell_interface.PassCmdError
        if the password command returns a non-zero exit code
    DeviceDecryptionError
        if cryptsetup returns a non-zero exit code
    """
    map_name = device.name
    decrypt_cmd: sh.StrPathList = ["sudo", "cryptsetup", "open", device, map_name]
    try:
        sh.pipe_pass_cmd_to_real_cmd(pass_cmd, decrypt_cmd)
    except sh.ShellInterfaceError as e:
        raise DeviceDecryptionError from e
    return Path("/dev/mapper/") / map_name


def close_decrypted_device(device: Path) -> None:
    """Close a decrypted device

    This function will try to close a device that was previously opened by
    `cryptsetup`. The given path must point into `/dev/mapper`, because
    `cryptsetup` always opens devices into there. If the given path points
    somewhere else, a InvalidDecryptedDevice is raised.

    Parameters:
    -----------
    device
        The device do be closed.

    Raises:
    -------
    InvalidDecryptedDevice
        if `device` does not point into `/dev/mapper`
    shell_interface.ShellInterfaceError
        if the exit code of the close command is non-zero
    """
    if device.parent != Path("/dev/mapper"):
        raise InvalidDecryptedDevice
    map_name = device.name
    close_cmd = ["sudo", "cryptsetup", "close", map_name]
    sh.run_cmd(cmd=close_cmd)


def encrypt_device(device: Path, password_cmd: str) -> UUID:
    """Encrypt a device

    This function will encrypt a device. The device can be any valid file-like
    object like real devices in `/dev/` or suitably sized files in $HOME.

    In order to retrieve the necessary password, the input `password_cmd` is
    executed in a subshell and its STDOUT used as password. Therefore, DO NOT
    USE UNTRUSTED `password_cmd`!

    In order to obtain a safe password_cmd, refer to `generate_passcmd`.

    Parameters:
    -----------
    device
        file-like object to be encrypted
    password_cmd
        Shell command that prints the password to be used to STDOUT

    Returns:
    --------
    UUID
        UUID of the new LUKS partition

    Raises:
    -------
    shell_interface.PassCmdError
        if the password command returns a non-zero exit code
    shell_interface.ShellInterfaceError
        if the cryptsetup command returns a non-zero exit code
    """
    new_uuid = uuid4()
    format_cmd: sh.StrPathList = [
        "sudo",
        "cryptsetup",
        "luksFormat",
        "--uuid",
        str(new_uuid),
        device,
    ]
    sh.pipe_pass_cmd_to_real_cmd(pass_cmd=password_cmd, command=format_cmd)
    return new_uuid


def get_filesystem(device: Path) -> str:
    """Get the file system type of a given device or path

    This function will query the file system type of the given device using
    `blkid`.

    Parameters:
    -----------
    device
        file-like object whose file system type to determine

    Returns:
    --------
    str
        the file system type (e.g. ``"btrfs"`` or ``"ext4"``)
    """
    cmd: sh.StrPathList = ["sudo", "blkid", "-o", "value", "-s", "TYPE", device]
    result = sh.run_cmd(cmd=cmd, capture_output=True)
    return result.stdout.decode().strip()


def mkfs_btrfs(device: Path) -> None:
    """Format device with BtrFS

    Parameters:
    -----------
    device
        file-like object to be formatted
    """

    cmd: sh.StrPathList = ["sudo", "mkfs.btrfs", device]
    sh.run_cmd(cmd=cmd)


def mkfs_ext4(device: Path) -> None:
    """Format device with ext4

    Parameters:
    -----------
    device
        file-like object to be formatted
    """

    cmd: sh.StrPathList = ["sudo", "mkfs.ext4", device]
    sh.run_cmd(cmd=cmd)


def mkfs(device: Path, filesystem: ValidFileSystems) -> None:
    """Format device with the given file system

    This function formats the given device with the specified file system,
    so the caller does not need to branch on the file system type themselves.

    Parameters:
    -----------
    device
        file-like object to be formatted
    filesystem
        the file system to use for formatting
    """
    match filesystem:
        case "btrfs":
            mkfs_btrfs(device)
        case "ext4":
            mkfs_ext4(device)
        case _:
            t.assert_never(filesystem)


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


def chown(
    file_or_folder: Path,
    /,
    user: int | str,
    group: int | str | None = None,
    *,
    recursive: bool,
) -> None:
    """Change user and group of a device or folder

    This function will change the ownership as specified. It requires root
    privileges and will ask for them if not available. If no group is given,
    only the owner is changed.

    If recursive is true, ownership information of all files and folders
    contained by `file_or_folder` will be adapted.

    If `file_or_folder` points to a file, `recursive` must be `False`.
    Otherwise a ValueError will be raised.


    Parameters:
    -----------
    user
        user ID, either as name or as UID
    group
        group ID, either as name or as GID
    recursive
        whether or not to change ownership for content

    Raises:
    --------
    ValueError
        if `file_or_folder` is a file but `recursive` is `True`
    """
    if file_or_folder.is_file() and recursive:
        raise ValueError(
            "First argument must point to a directory if `recursive` is `True`!"
        )

    user_spec = str(user) if group is None else f"{user}:{group}"
    chown_cmd: sh.StrPathList = ["sudo", "chown", user_spec, file_or_folder]
    if recursive:
        chown_cmd.append("--recursive")
    sh.run_cmd(cmd=chown_cmd)
