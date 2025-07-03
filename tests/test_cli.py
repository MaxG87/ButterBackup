import re
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from unittest import mock

import pytest
import shell_interface as sh
import storage_device_managers as sdm
from loguru import logger
from typer.testing import CliRunner

from butter_backup import cli
from butter_backup import config_parser as cp
from butter_backup.cli import app
from tests import complement_configuration, get_random_filename

BY_UUID = Path("/dev/disk/by-uuid")
print(f"{BY_UUID=}")
print(f"{list(BY_UUID.iterdir())=}")


def in_docker_container() -> bool:
    return Path("/.dockerenv").exists()


@pytest.fixture
def runner():
    return CliRunner()


def test_get_default_config_path() -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    with TemporaryDirectory() as tempdir:
        xdg_config_dir = Path(tempdir)
    with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config_dir}.get):
        config_file = cli.get_default_config_path()
    expected_cfg = xdg_config_dir / cli.DEFAULT_CONFIG_NAME
    assert str(expected_cfg) == config_file
    print(f"{list(BY_UUID.iterdir())=}")


@pytest.mark.parametrize(
    "logmsg",
    [
        "Schläft ein Lied in allen Dingen,",
        "Die da träumen fort und fort,",
        "Und die Welt hebt an zu singen,",
        "Triffst du nur das Zauberwort.",
    ],
)
@pytest.mark.parametrize("logfunc", [logger.warning, logger.error])
def test_setup_logging_logs_errors_and_warnings_by_default(
    logmsg: str, logfunc, capsys
) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    cli.setup_logging(verbosity=0)
    logfunc(logmsg)
    out, err = capsys.readouterr()
    err_without_linebreak = err[:-1]
    assert out == ""
    assert err_without_linebreak.endswith(logmsg)
    print(f"{list(BY_UUID.iterdir())=}")


def test_setup_logging_does_not_log_more_than_warnings_by_default(capsys) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    cli.setup_logging(verbosity=0)
    logger.success("This line will not appear anywhere.")
    out, err = capsys.readouterr()
    assert out == ""
    assert err == ""
    print(f"{list(BY_UUID.iterdir())=}")


def test_setup_logging_logs_success(capsys) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    successmsg = "☕️🤎📰📜⚰️🕰🕯🎻🖋"
    infomsg = "🦖🦕🐊"
    cli.setup_logging(verbosity=1)
    logger.success(successmsg)
    logger.info(infomsg)
    out, err = capsys.readouterr()
    err_without_linebreak = err[:-1]
    assert out == ""
    assert infomsg not in err
    assert err_without_linebreak.endswith(successmsg)
    print(f"{list(BY_UUID.iterdir())=}")


def test_setup_logging_clamps_level(capsys) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    successmsg = "√-1 2³ Σ π and it was delicious"
    tracemsg = "Trace me if you can!"
    cli.setup_logging(verbosity=1337)
    logger.success(successmsg)
    logger.trace(tracemsg)
    out, err = capsys.readouterr()
    assert out == ""
    assert successmsg in err
    assert tracemsg in err
    print(f"{list(BY_UUID.iterdir())=}")


@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_missing_config(subprogram, runner) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    config_file = Path(get_random_filename())
    result = runner.invoke(app, [subprogram, "--config", str(config_file)])
    assert f"{config_file}" in result.stderr
    assert result.exit_code != 0
    print(f"{list(BY_UUID.iterdir())=}")


@pytest.mark.skipif(in_docker_container(), reason="All files are readable for root")
@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_unreadable_file(subprogram, runner) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    with NamedTemporaryFile() as fh:
        config_file = Path(fh.name)
        config_file.chmod(0)
        result = runner.invoke(app, [subprogram, "--config", str(config_file)])
        assert f"{config_file}" in result.stderr
        assert result.exit_code != 0
    print(f"{list(BY_UUID.iterdir())=}")


@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_directories(subprogram, runner) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    with TemporaryDirectory() as tmp_dir:
        result = runner.invoke(app, [subprogram, "--config", tmp_dir])
        assert tmp_dir in result.stderr
        assert result.exit_code != 0
    print(f"{list(BY_UUID.iterdir())=}")


@pytest.mark.skip("Impossible to implement!")
def test_open_refuses_missing_xdg_config(runner) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    # It seems as if this test cannot be implemented at the moment.
    #
    # This test resets XDG_CONFIG_HOME to provoke that get_default_config_path
    # returns a not existing config file. However, get_default_config_path is
    # executed at import time, rendering resetting XDG_CONFIG_HOME effectless.
    with TemporaryDirectory() as xdg_config_dir:
        pass
    with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config_dir}.get):
        result = runner.invoke(app, ["open"])
    assert xdg_config_dir in result.stderr
    assert result.exit_code != 0
    print(f"{list(BY_UUID.iterdir())=}")


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


@pytest.mark.parametrize(
    "backend", ["BackupBackend", "fvglxvleaeb", "NotYetImplementedBackend"]
)
def test_format_device_refuses_incorrect_backend(runner, backend: str) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    with NamedTemporaryFile() as tempf:
        result = runner.invoke(app, ["format-device", tempf.name, backend])
        assert result.exit_code != 0
    print(f"{list(BY_UUID.iterdir())=}")


@pytest.mark.parametrize("backend", ["restic", "btrfs-rsync"])
def test_format_device(runner, backend: str, big_file: Path) -> None:
    print(f"{list(BY_UUID.iterdir())=}")
    format_result = runner.invoke(app, ["format-device", backend, str(big_file)])
    print(f"{format_result.stdout=}")
    print(f"{format_result.stderr=}")
    serialised_config = format_result.stdout
    config_lst = list(cp.parse_configuration(serialised_config))
    print(f"{config_lst=}")
    assert len(config_lst) == 1
    device_uuid = config_lst[0].UUID
    link_dest = Path(f"/dev/disk/by-uuid/{device_uuid}")
    print(f"{link_dest.exists()=}")
    print(f"{link_dest.is_symlink()=}")
    sibblings = list(link_dest.parent.iterdir())
    print(f"{sibblings=}")
    with NamedTemporaryFile("w") as fh:
        fh.write(serialised_config)
        fh.seek(0)
        print(f"{fh.name=}")
        with sdm.symbolic_link(big_file, Path(f"/dev/disk/by-uuid/{device_uuid}")):
            print("Symbolic link created successfully.")
            open_result = runner.invoke(app, ["open", "--config", fh.name])
            close_result = runner.invoke(app, ["close", "--config", fh.name])
    assert format_result.exit_code == 0
    assert open_result.exit_code == 0
    assert close_result.exit_code == 0
    assert str(device_uuid) in open_result.stdout
    print(f"{list(BY_UUID.iterdir())=}")


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
