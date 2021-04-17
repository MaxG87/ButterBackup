from pathlib import Path
from unittest import mock

from hypothesis import given
from hypothesis import strategies as st

from butter_backup import __main__ as bb

path_to_config_files = st.text(min_size=1)


def test_no_args_parsing() -> None:
    default_config = bb.DEFAULT_CONFIG
    with mock.patch("sys.argv", ["butter-backup"]):
        config = bb.parse_args()
    assert config == default_config


@given(cfg=path_to_config_files, xdg_config=path_to_config_files)
def test_parse_args_returns_passed_file(cfg: str, xdg_config: str) -> None:
    with mock.patch("sys.argv", ["butter-backup", "--config", cfg]):
        with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config}.get):
            parsed_config = bb.parse_args()
    assert Path(cfg) == parsed_config


@given(xdg_config=path_to_config_files)
def test_parse_args_returns_xdg_config_home(xdg_config: str) -> None:
    with mock.patch("sys.argv", ["butter-backup"]):
        with mock.patch("os.getenv", {"XDG_CONFIG_HOME": xdg_config}.get):
            parsed_config = bb.parse_args()
    assert Path(xdg_config) / bb.DEFAULT_CONFIG_NAME == parsed_config
