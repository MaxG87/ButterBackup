from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest

from butter_backup.cli import app
from tests import get_random_filename

from . import assert_is_error_result, in_docker_container


@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_missing_config(subprogram, runner) -> None:
    config_file = Path(get_random_filename())
    result = runner.invoke(app, [subprogram, "--config", str(config_file)])
    assert f"{config_file}" in result.stderr
    assert_is_error_result(result, expected_exit_code=2)


@pytest.mark.skipif(in_docker_container(), reason="All files are readable for root")
@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_unreadable_file(subprogram, runner) -> None:
    with NamedTemporaryFile(suffix=".json") as fh:
        config_file = Path(fh.name)
        config_file.chmod(0)
        result = runner.invoke(app, [subprogram, "--config", str(config_file)])
        assert f"{config_file}" in result.stderr
        assert_is_error_result(result, expected_exit_code=2)


@pytest.mark.parametrize(
    "subprogram",
    ["backup", "close", "open"],
)
def test_subprograms_refuse_directories(subprogram, runner, tmp_path: Path) -> None:
    tmp_path_as_str = str(tmp_path)
    result = runner.invoke(app, [subprogram, "--config", tmp_path_as_str])
    assert tmp_path_as_str in result.stderr
    assert_is_error_result(result, expected_exit_code=2)


def test_open_refuses_missing_xdg_config(runner, tmp_path, monkeypatch) -> None:
    xdg_config_dir = tmp_path / "nonexistent_config_dir"
    monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg_config_dir))
    result = runner.invoke(app, ["open"])
    assert str(xdg_config_dir) in result.stderr
    assert_is_error_result(result, expected_exit_code=2)
