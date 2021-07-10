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
        UUID=st.text(),
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


@given(
    config=st.builds(
        dict,
        UUID=st.text(),
        PassCmd=st.text(),
        Routes=st.lists(
            st.lists(st.text(), min_size=NOF_ROUTES_ELEMS, max_size=NOF_ROUTES_ELEMS)
        ).map(tuple),
    )
)
def test_parsing_config_parses(config) -> None:
    cfg = bb.ParsedButterConfig.from_dict(config)
    assert cfg.uuid == config["UUID"]
    assert cfg.pass_cmd == config["PassCmd"]
    for src, dest in cfg.routes:
        assert [src, dest] in config["Routes"]
