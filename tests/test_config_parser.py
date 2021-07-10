from __future__ import annotations

from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

import pytest
from hypothesis import given
from hypothesis import strategies as st

from butter_backup import __main__ as bb

SUCCESS_CODES = {0, None}
NOF_ROUTES_ELEMS = 2
EXPECTED_CFG_KEYS = sorted({"UUID", "PassCmd", "Routes"})

valid_uuids = st.text(
    st.characters(whitelist_categories=["Nd", "Lu", "Ll"]), min_size=1
)
valid_unparsed_configs = st.builds(
    dict,
    UUID=valid_uuids,
    PassCmd=st.text(),
    Routes=st.lists(
        st.lists(
            st.text(min_size=1), min_size=NOF_ROUTES_ELEMS, max_size=NOF_ROUTES_ELEMS
        )
    ).map(tuple),
)


def test_load_configuration_rejects_missing_cfg() -> None:
    with NamedTemporaryFile() as named_tmp:
        file_name = Path(named_tmp.name)
    with pytest.raises(SystemExit) as sysexit:
        bb.load_configuration(file_name)
    assert sysexit.value.code not in SUCCESS_CODES


def test_load_configuration_rejects_empty_file() -> None:
    with TemporaryDirectory() as td:
        file_name = Path(td, "configuration")
        file_name.write_text("[]")
        with pytest.raises(SystemExit) as sysexit:
            bb.load_configuration(file_name)
        assert sysexit.value.code not in SUCCESS_CODES


@given(
    incomplete_cfg=st.dictionaries(
        keys=st.sampled_from(EXPECTED_CFG_KEYS),
        values=st.text(),
        max_size=len(EXPECTED_CFG_KEYS) - 1,
    )
)
def test_parsing_config_fails_on_missing_keys(incomplete_cfg) -> None:
    with pytest.raises(SystemExit) as sysexit:
        bb.ParsedButterConfig.from_dict(incomplete_cfg)
    assert sysexit.value.code not in SUCCESS_CODES


@given(
    config=st.builds(
        dict,
        UUID=valid_uuids,
        PassCmd=st.text(),
        Routes=st.lists(st.lists(st.text()))
        .filter(lambda lst: not all(len(tup) == NOF_ROUTES_ELEMS for tup in lst))
        .map(tuple),
    )
)
def test_parsing_config_fails_on_malformed_routes(config) -> None:
    with pytest.raises(SystemExit) as sysexit:
        bb.ParsedButterConfig.from_dict(config)
    assert sysexit.value.code not in SUCCESS_CODES


@given(config=valid_unparsed_configs)
def test_parsing_config_parses(config) -> None:
    cfg = bb.ParsedButterConfig.from_dict(config)
    assert cfg.uuid == config["UUID"]
    assert cfg.pass_cmd == config["PassCmd"]
    for src, dest in cfg.routes:
        assert [src, dest] in config["Routes"]


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_accepts_raw_config(uuid: str, pass_cmd: str):
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as dest:
            raw_config = bb.ParsedButterConfig(
                uuid=uuid, pass_cmd=pass_cmd, routes=[(src, dest)]
            )
            cfg = bb.ButterConfig.from_raw_config(raw_config)
    assert cfg.pass_cmd == raw_config.pass_cmd
    assert str(cfg.device).endswith(raw_config.uuid)
    assert [(str(cfg.routes[0][0]), str(cfg.routes[0][1]))] == raw_config.routes


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_rejects_missing_src(uuid: str, pass_cmd: str):
    with TemporaryDirectory() as src:
        with TemporaryDirectory() as dest:
            pass
    routes = [
        ("/usr/bin", "backup_bins"),
        (src, dest),
        ("/var/log", "backup_logs"),
    ]
    raw_config = bb.ParsedButterConfig(uuid=uuid, pass_cmd=pass_cmd, routes=routes)
    with pytest.raises(SystemExit) as sysexit:
        bb.ButterConfig.from_raw_config(raw_config)
    assert sysexit.value.code not in SUCCESS_CODES
    assert uuid in sysexit.value.code  # type: ignore
    assert src in sysexit.value.code  # type: ignore


@given(
    uuid=valid_uuids,
    pass_cmd=st.text(),
)
def test_butter_config_rejects_non_dir_src(uuid: str, pass_cmd: str):
    with NamedTemporaryFile() as src:
        with TemporaryDirectory() as dest:
            pass
        routes = [
            ("/usr/bin", "backup_bins"),
            (src.name, dest),
            ("/var/log", "backup_logs"),
        ]
        raw_config = bb.ParsedButterConfig(uuid=uuid, pass_cmd=pass_cmd, routes=routes)
        with pytest.raises(SystemExit) as sysexit:
            bb.ButterConfig.from_raw_config(raw_config)
    assert sysexit.value.code not in SUCCESS_CODES
    assert uuid in sysexit.value.code  # type: ignore
    assert src.name in sysexit.value.code  # type: ignore
