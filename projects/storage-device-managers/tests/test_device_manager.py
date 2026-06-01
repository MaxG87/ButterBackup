from __future__ import annotations

import os
from collections import Counter
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory
from uuid import UUID

import pytest
import shell_interface as sh
from hypothesis import given
from hypothesis import strategies as st

import storage_device_managers as sdm


def get_random_filename() -> str:
    with NamedTemporaryFile() as named_file:
        return named_file.name


class MyCustomTestException(Exception):
    pass


def test_open_encrypted_device_raises_devicedecryptionerror() -> None:
    pass_cmd = sdm.generate_passcmd()
    with NamedTemporaryFile() as named_temp_file:
        device = Path(named_temp_file.name)
        with pytest.raises(sdm.DeviceDecryptionError):
            sdm.open_encrypted_device(device=device, pass_cmd=pass_cmd)


def test_decrypted_device_raises_devicedecryptionerror() -> None:
    pass_cmd = sdm.generate_passcmd()
    with NamedTemporaryFile() as named_temp_file:
        device = Path(named_temp_file.name)
        with pytest.raises(sdm.DeviceDecryptionError):
            with sdm.decrypted_device(device=device, pass_cmd=pass_cmd):
                pass


def test_decrypt_device_roundtrip(encrypted_device) -> None:
    device, pass_cmd = encrypted_device
    decrypted = sdm.open_encrypted_device(device=device, pass_cmd=pass_cmd)
    assert decrypted.exists()
    assert decrypted.name == device.name
    sdm.close_decrypted_device(device=decrypted)
    assert not decrypted.exists()


@given(uuid=st.uuids())
def test_close_decrypted_device_rejects_invalid_device_name(uuid) -> None:
    with TemporaryDirectory() as td:
        device = Path(td) / str(uuid)
        with pytest.raises(sdm.InvalidDecryptedDevice):
            sdm.close_decrypted_device(device)


def test_decrypted_device(encrypted_device) -> None:
    device, pass_cmd = encrypted_device
    with sdm.decrypted_device(device=device, pass_cmd=pass_cmd) as dd:
        assert dd.exists()
    assert not dd.exists()


def test_decrypted_device_closes_in_case_of_exception(encrypted_device) -> None:
    device, pass_cmd = encrypted_device
    with pytest.raises(MyCustomTestException):
        with sdm.decrypted_device(device=device, pass_cmd=pass_cmd) as dd:
            raise MyCustomTestException
    assert not dd.exists()


def test_decrypted_device_can_use_home_for_passcmd(encrypted_device) -> None:
    # Regression Test
    # Test if `decrypted_device` can use a program that is located in PATH. For
    # some reason, when passing `{}` as environment, `echo` works, but `pass`
    # did not. This test ensures that the necessary fix is not reverted again.
    device, pass_cmd = encrypted_device
    passphrase = pass_cmd.split()[-1]
    relative_home = Path("~")  # must be relative to trigger regression
    with NamedTemporaryFile(dir=relative_home.expanduser()) as pwd_f:
        absolute_pwd_f = Path(pwd_f.name)
        relative_pwd_f = relative_home / absolute_pwd_f.name
        absolute_pwd_f.write_text(passphrase)
        with sdm.decrypted_device(
            device=device, pass_cmd=f"cat {relative_pwd_f}"
        ) as dd:
            assert dd.exists()
        assert not dd.exists()


def test_symbolic_link_rejects_existing_dest(tmp_path: Path) -> None:
    with NamedTemporaryFile() as named_file:
        source = Path(named_file.name)
        with pytest.raises(FileExistsError):
            with sdm.symbolic_link(source, dest=tmp_path):
                pass


def test_symbolic_link_rejects_missing_src() -> None:
    src = Path(get_random_filename())
    dest = Path(get_random_filename())
    with pytest.raises(FileNotFoundError):
        with sdm.symbolic_link(src=src, dest=dest):
            pass


def test_symbolic_link() -> None:
    content = "some arbitrary content"
    with NamedTemporaryFile() as named_file:
        source = Path(named_file.name)
        source.write_text(content)
        in_dest = Path(get_random_filename())
        with sdm.symbolic_link(src=source, dest=in_dest) as out_dest:
            assert in_dest == out_dest
            assert out_dest.is_symlink()
            assert out_dest.read_bytes() == source.read_bytes()
        assert not out_dest.exists()


def test_symbolic_link_removes_link_in_case_of_exception() -> None:
    with pytest.raises(MyCustomTestException):
        with NamedTemporaryFile() as src_f:
            source = Path(src_f.name)
            dest_p = Path(get_random_filename())
            assert not os.path.lexists(dest_p)
            with sdm.symbolic_link(src=source, dest=dest_p):
                assert os.path.lexists(dest_p)
                raise MyCustomTestException
    assert not os.path.lexists(dest_p)


def test_symbolic_link_does_not_crash_in_case_of_vanished_link() -> None:
    content = "some arbitrary content"
    with NamedTemporaryFile() as named_file:
        source = Path(named_file.name)
        source.write_text(content)
        in_dest = Path(get_random_filename())
        with sdm.symbolic_link(src=source, dest=in_dest) as out_dest:
            cmd: sh.StrPathList = ["sudo", "rm", out_dest]
            sh.run_cmd(cmd=cmd)


def test_generate_passcmd_is_not_static():
    N = 128
    passwords = Counter(sdm.generate_passcmd() for _ in range(N))
    assert set(passwords.values()) == {1}


def test_generate_passcmd_samples_uniformly():
    # TODO: The bounds should be tightened. Finally, the test should have a
    # probability to fail one in ~100 runs or so.
    N = 128
    chars: Counter[str] = Counter()
    for _ in range(N):
        passcmd = sdm.generate_passcmd()
        passphrase = passcmd.split()[-1]
        chars.update(passphrase)

    nof_chars = sum(chars.values())
    expected_frequency = 1 / (26 + 26 + 10)
    lower_bound = 0.25 * expected_frequency
    upper_bound = 1.75 * expected_frequency
    assert all(len(char) == 1 for char in chars)
    assert all(lower_bound < (cur / nof_chars) < upper_bound for cur in chars.values())


def test_encrypt_device_reports_correct_uuid(big_file) -> None:
    pass_cmd = sdm.generate_passcmd()
    result_uuid = sdm.encrypt_device(big_file, pass_cmd)
    uuid_check_cmd = ["sudo", "cryptsetup", "luksUUID", big_file]
    uuid_check_proc = sh.run_cmd(cmd=uuid_check_cmd, capture_output=True)
    reported_uuid = UUID(uuid_check_proc.stdout.decode().strip())
    assert result_uuid == reported_uuid


def test_open_encrypted_device_raises_passcmderror_on_bad_pass_cmd(
    encrypted_device,
) -> None:
    device, _ = encrypted_device
    bad_pass_cmd = "exit 1"
    with pytest.raises(sh.PassCmdError):
        sdm.open_encrypted_device(device=device, pass_cmd=bad_pass_cmd)


def test_decrypted_device_raises_passcmderror_on_bad_pass_cmd(
    encrypted_device,
) -> None:
    device, _ = encrypted_device
    bad_pass_cmd = "exit 1"
    with pytest.raises(sh.PassCmdError):
        with sdm.decrypted_device(device=device, pass_cmd=bad_pass_cmd):
            pass


def test_encrypt_device_raises_passcmderror_on_bad_pass_cmd(big_file) -> None:
    bad_pass_cmd = "exit 1"
    with pytest.raises(sh.PassCmdError):
        sdm.encrypt_device(big_file, bad_pass_cmd)
