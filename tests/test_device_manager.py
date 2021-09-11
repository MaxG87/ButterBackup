import subprocess
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest

from butter_backup import device_managers as dm


def test_mounted_device(btrfs_device) -> None:
    with dm.mounted_device(btrfs_device) as md:
        assert md.exists()
        assert md.is_dir()
        assert dm.is_mounted(btrfs_device)
        assert md in dm.get_mounted_devices()[str(btrfs_device)]
    assert not md.exists()
    assert not dm.is_mounted(btrfs_device)
    assert str(btrfs_device) not in dm.get_mounted_devices()


def test_mounted_device_takes_over_already_mounted_device(btrfs_device) -> None:
    with TemporaryDirectory() as td:
        dm.mount_btrfs_device(btrfs_device, Path(td))
        with dm.mounted_device(btrfs_device) as md:
            assert dm.is_mounted(btrfs_device)
            assert md in dm.get_mounted_devices()[str(btrfs_device)]
        assert not dm.is_mounted(btrfs_device)


def test_mounted_device_fails_on_not_unmountable_device() -> None:
    for device, mount_points in dm.get_mounted_devices().items():
        if Path("/") in mount_points:
            root = Path(device)
            break
    else:
        assert False, "Device of / not found!"
    with pytest.raises(subprocess.CalledProcessError):
        with dm.mounted_device(root):
            pass


@pytest.mark.parametrize("device", dm.get_mounted_devices())
def test_is_mounted_detects(device: Path) -> None:
    assert dm.is_mounted(device)


def test_is_mounted_rejects() -> None:
    with TemporaryDirectory() as tempd:
        assert not dm.is_mounted(Path(tempd))


def test_get_mounted_devices_raises_on_unknown_device() -> None:
    with pytest.raises(KeyError):
        dm.get_mounted_devices()["unknown-device"]


def test_get_mounted_devices_includes_correct_mountpoints(mounted_directories) -> None:
    src, dest = mounted_directories
    assert any(
        dest in mount_points for mount_points in dm.get_mounted_devices().values()
    )


def test_get_mounted_devices_includes_root() -> None:
    assert any(Path("/") in dest_set for dest_set in dm.get_mounted_devices().values())


def test_unmount_device(btrfs_device) -> None:
    with TemporaryDirectory() as mountpoint:
        dm.mount_btrfs_device(btrfs_device, Path(mountpoint))
        dm.unmount_device(btrfs_device)
        assert not dm.is_mounted(btrfs_device)


def test_decrypted_device(encrypted_device) -> None:
    map_name = "decrypted_device_test"
    passphrase, device = encrypted_device
    with dm.decrypted_device(
        device=device, map_name=map_name, pass_cmd=f"echo {passphrase}"
    ) as dd:
        assert dd.exists()
    assert not dd.exists()


def test_decrypted_device_can_use_home_for_passcmd(encrypted_device) -> None:
    # Regression Test
    # Test if `decrypted_device` can use a program that is located in PATH. For
    # some reason, when passing `{}` as environment, `echo` works, but `pass`
    # did not. This test ensures that the necessary fix is not reverted again.
    map_name = "decrypted_device_test"
    passphrase, device = encrypted_device
    relative_home = Path("~")  # must be relative to trigger regression
    with NamedTemporaryFile(dir=relative_home.expanduser()) as pwd_f:
        absolute_pwd_f = Path(pwd_f.name)
        relative_pwd_f = relative_home / absolute_pwd_f.name
        absolute_pwd_f.write_text(passphrase)
        with dm.decrypted_device(
            device=device, map_name=map_name, pass_cmd=f"cat {relative_pwd_f}"
        ) as dd:
            assert dd.exists()
        assert not dd.exists()


def test_symbolic_link_rejects_existing_dest(tmp_path: Path) -> None:
    with NamedTemporaryFile() as named_file:
        source = Path(named_file.name)
        with pytest.raises(FileExistsError):
            with dm.symbolic_link(source, dest=tmp_path):
                pass


def test_symbolic_link_rejects_missing_src() -> None:
    with NamedTemporaryFile() as named_file:
        src = Path(named_file.name)
    with NamedTemporaryFile() as named_file:
        dest = Path(named_file.name)
    with pytest.raises(FileNotFoundError):
        with dm.symbolic_link(src=src, dest=dest):
            pass


def test_symbolic_link() -> None:
    content = "some arbitrary content"
    with NamedTemporaryFile() as named_file:
        source = Path(named_file.name)
        source.write_text(content)
        with NamedTemporaryFile() as named_file:
            in_dest = Path(named_file.name)
        with dm.symbolic_link(src=source, dest=in_dest) as out_dest:
            assert in_dest == out_dest
            assert out_dest.is_symlink()
            assert out_dest.read_bytes() == source.read_bytes()
        assert not out_dest.exists()
