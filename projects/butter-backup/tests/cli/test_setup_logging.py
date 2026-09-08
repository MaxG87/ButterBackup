import pytest
from loguru import logger

from butter_backup import cli


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
    cli.setup_logging(verbosity=0)
    logfunc(logmsg)
    out, err = capsys.readouterr()
    err_without_linebreak = err[:-1]
    assert out == ""
    assert err_without_linebreak.endswith(logmsg)


def test_setup_logging_does_not_log_more_than_warnings_by_default(capsys) -> None:
    cli.setup_logging(verbosity=0)
    logger.success("This line will not appear anywhere.")
    out, err = capsys.readouterr()
    assert out == ""
    assert err == ""


def test_setup_logging_logs_success(capsys) -> None:
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


def test_setup_logging_clamps_level(capsys) -> None:
    successmsg = "√-1 2³ Σ π and it was delicious"
    tracemsg = "Trace me if you can!"
    cli.setup_logging(verbosity=1337)
    logger.success(successmsg)
    logger.trace(tracemsg)
    out, err = capsys.readouterr()
    assert out == ""
    assert successmsg in err
    assert tracemsg in err
