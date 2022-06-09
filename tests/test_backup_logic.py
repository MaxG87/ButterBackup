from butter_backup import backup_logic as bl
from butter_backup import device_managers as dm


def test_do_backup_for_butterbackend(encrypted_btrfs_device) -> None:
    config, device = encrypted_btrfs_device
    bl.do_backup(config)
    with dm.decrypted_device(device, config.DevicePassCmd) as decrypted:
        with dm.mounted_device(decrypted) as mount_dir:
            latest_folder = sorted(mount_dir.iterdir())[-1]
            content = {file: file.read_bytes() for file in latest_folder.iterdir()}
    assert content == {}
