import datetime as dt

from butter_backup import backup_logic as bl
from butter_backup import device_managers as dm


def test_do_backup_for_butterbackend(encrypted_btrfs_device) -> None:
    # Due to testing a timestamp, this test has a small chance to fail around
    # midnight. Since this is a hobby project, paying attention if the test
    # executes around midnight is a viable solution.
    config, device = encrypted_btrfs_device
    bl.do_backup(config)
    with dm.decrypted_device(device, config.DevicePassCmd) as decrypted:
        with dm.mounted_device(decrypted) as mount_dir:
            latest_folder = sorted(mount_dir.iterdir())[-1]
            content = {file: file.read_bytes() for file in latest_folder.iterdir()}
    expected_date = dt.date.today().isoformat()
    assert expected_date in str(latest_folder)
    assert content == {}
