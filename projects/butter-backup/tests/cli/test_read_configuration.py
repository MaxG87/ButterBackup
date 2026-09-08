from pathlib import Path
from uuid import UUID

import pytest
import typer

from butter_backup import cli
from butter_backup import config_parser as cp


def test_read_configuration_uses_first_matching_default_file(
    tmp_path: Path, monkeypatch
) -> None:
    tempdir = tmp_path
    xdg_config_dir = Path(tempdir)
    butter_backup_config_dir = xdg_config_dir / "butter-backup"
    json5_config_f = butter_backup_config_dir / "config.json5"
    toml_config_f = butter_backup_config_dir / "config.toml"

    json5_cfg = cp.Configuration(
        DeviceConfigurations=[
            cp.ResticConfig(
                Name="restic",
                UUID=UUID("12345678-1234-5678-1234-567812345678"),
                DevicePassCmd="echo pw",
                BackupRepositoryFolder="repo",
                RepositoryPassCmd="echo rpw",
                FilesAndFolders={Path("/tmp")},
            )
        ]
    )
    toml_cfg = cp.Configuration(
        DeviceConfigurations=[
            cp.ResticConfig(
                Name="toml",
                UUID=UUID("87654321-4321-8765-4321-876543218765"),
                DevicePassCmd="echo pw",
                BackupRepositoryFolder="repo",
                RepositoryPassCmd="echo rpw",
                FilesAndFolders={Path("/tmp")},
            )
        ]
    )
    butter_backup_config_dir.mkdir()
    json5_config_f.write_text(json5_cfg.model_dump_json())
    toml_config_f.write_text(
        """
[butter-backup]
[[butter-backup.device-configurations]]
Name = "toml"
UUID = "87654321-4321-8765-4321-876543218765"
DevicePassCmd = "echo pw"
BackupRepositoryFolder = "repo"
RepositoryPassCmd = "echo rpw"
FilesAndFolders = ["/tmp"]
"""
    )
    monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg_config_dir))
    loaded = cli._read_configuration(None)
    assert loaded == json5_cfg
    assert loaded != toml_cfg


@pytest.mark.parametrize("suffix", cli.DEFAULT_CONFIG_SUFFIX_ORDER)
def test_read_configuration_rejects_uppercase_extension(
    tmp_path: Path, suffix: str
) -> None:
    uppercase_suffix = suffix.upper()
    cfg_file = tmp_path / f"butter-backup.{uppercase_suffix}"
    cfg_file.write_text("{}")
    with pytest.raises(typer.BadParameter):
        cli._read_configuration(cfg_file)
