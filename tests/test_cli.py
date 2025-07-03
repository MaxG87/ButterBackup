import re
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest
import shell_interface as sh
import storage_device_managers as sdm
from typer.testing import CliRunner

from butter_backup import config_parser as cp
from butter_backup.cli import app
from tests import complement_configuration

BY_UUID = Path("/dev/disk/by-uuid")
print(f"{BY_UUID=}")
print(f"{list(BY_UUID.iterdir())=}")


def in_docker_container() -> bool:
    return Path("/.dockerenv").exists()


@pytest.fixture
def runner():
    return CliRunner()


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_close_does_not_close_unopened_device(runner, encrypted_btrfs_device) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    config = encrypted_btrfs_device
    with NamedTemporaryFile() as tempf:
        config_file = Path(tempf.name)
        config_file.write_text(f"[{config.model_dump_json()}]")
        close_result = runner.invoke(app, ["close", "--config", str(config_file)])
        assert close_result.stdout == ""
        assert close_result.exit_code == 0
    print(f"{list(BY_UUID.iterdir())=}")


@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_open_close_roundtrip(runner, encrypted_device) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    config = encrypted_device
    expected_cryptsetup_map = Path(f"/dev/mapper/{config.UUID}")
    with NamedTemporaryFile() as tempf:
        config_file = Path(tempf.name)
        config_file.write_text(f"[{config.model_dump_json()}]")
        open_result = runner.invoke(app, ["open", "--config", str(config_file)])
        expected_msg = (
            f"Speichermedium {config.UUID} wurde in (?P<mount_dest>/[^ ]+) geöffnet."
        )
        match = re.fullmatch(expected_msg, open_result.stdout.strip())
        assert match is not None
        mount_dest = Path(match.group("mount_dest"))
        assert any(
            mount_dest in destinations
            for destinations in sdm.get_mounted_devices().values()
        )
        assert expected_cryptsetup_map.exists()
        runner.invoke(app, ["close", "--config", str(config_file)])
        assert not expected_cryptsetup_map.exists()
        assert not sdm.is_mounted(mount_dest)
        assert not mount_dest.exists()
    print(f"{list(BY_UUID.iterdir())=}")


@pytest.mark.parametrize("backend", ["restic", "btrfs-rsync"])
def test_format_device(runner, backend: str, big_file: Path) -> None:
    print(f"01 - {list(BY_UUID.iterdir())=}")
    format_result = runner.invoke(app, ["format-device", backend, str(big_file)])
    print(f"02 - {format_result.stdout=}")
    print(f"03 - {format_result.stderr=}")
    print(f"04 - {list(BY_UUID.iterdir())=}")
    serialised_config = format_result.stdout
    print(f"05 - {list(BY_UUID.iterdir())=}")
    config_lst = list(cp.parse_configuration(serialised_config))
    print(f"06 - {list(BY_UUID.iterdir())=}")
    print(f"07 - {config_lst=}")
    assert len(config_lst) == 1
    device_uuid = config_lst[0].UUID
    print(f"08 - {list(BY_UUID.iterdir())=}")
    link_dest = Path(f"/dev/disk/by-uuid/{device_uuid}")
    print(f"09 - {list(BY_UUID.iterdir())=}")
    print(f"10 - {link_dest=}")
    print(f"11 - {link_dest.exists()=}")
    print(f"12 - {link_dest.is_symlink()=}")
    sibblings = list(link_dest.parent.iterdir())
    print(f"13 - {sibblings=}")
    with NamedTemporaryFile("w") as fh:
        fh.write(serialised_config)
        fh.seek(0)
        print(f"14 - {fh.name=}")
        with sdm.symbolic_link(big_file, Path(f"/dev/disk/by-uuid/{device_uuid}")):
            print("Symbolic link created successfully.")
            open_result = runner.invoke(app, ["open", "--config", fh.name])
            close_result = runner.invoke(app, ["close", "--config", fh.name])
    assert format_result.exit_code == 0
    assert open_result.exit_code == 0
    assert close_result.exit_code == 0
    assert str(device_uuid) in open_result.stdout
    print(f"15 - {list(BY_UUID.iterdir())=}")


@pytest.mark.parametrize("backend", ["restic", "btrfs-rsync"])
def test_format_device_chowns_filesystem_to_user(
    runner, backend: str, big_file: Path
) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    format_result = runner.invoke(app, ["format-device", backend, str(big_file)])
    serialised_config = format_result.stdout
    config_lst = list(cp.parse_configuration(serialised_config))
    assert len(config_lst) == 1
    config = config_lst[0]
    print(f"{list(BY_UUID.iterdir())=}")

    with sdm.decrypted_device(big_file, config.DevicePassCmd) as decrypted:
        with sdm.mounted_device(decrypted, sdm.ValidCompressions.ZLIB1) as mounted:
            owner = mounted.owner()
            group = mounted.group()
    expected_user = sh.get_user()
    expected_group = sh.get_group(expected_user)
    assert owner == expected_user
    assert group == expected_group


@pytest.mark.parametrize("subprogram", ["open", "backup"])
@pytest.mark.skipif(
    in_docker_container(), reason="Test is known to fail in Docker container"
)
def test_do_backup_refuses_backup_when_device_is_already_open(
    subprogram: str, runner: CliRunner, encrypted_device, tmp_path: Path
) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    config = complement_configuration(encrypted_device, tmp_path)
    config_file = tmp_path / "config.json"
    print(f"{list(BY_UUID.iterdir())=}")

    config_file.write_text(f"[{config.model_dump_json()}]")
    runner.invoke(app, ["open", "--config", str(config_file)])
    result = runner.invoke(app, [subprogram, "--config", str(config_file)])
    expected_msg = (
        f"Speichermedium {config.UUID} ist bereits geöffnet. Es wird übersprungen."
    )

    assert result.exit_code == 0
    assert expected_msg in result.stderr
