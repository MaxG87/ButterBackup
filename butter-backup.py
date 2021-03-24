#!/usr/bin/env python3

from __future__ import annotations

import datetime as dt
import json
import subprocess
import sys
from argparse import ArgumentParser
from dataclasses import dataclass
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any, Optional


@dataclass
class DecryptedDevice:
    device: Path
    map_name: str
    password: str

    def __enter__(self) -> Path:
        decrypt_cmd = f"sudo cryptsetup open '{self.device}' '{self.map_name}'"
        subprocess.run(decrypt_cmd, check=True, input=self.password.encode(), shell=True)
        return Path(f"/dev/mapper/{self.map_name}")

    def __exit__(self, exc, value, tb):
        decrypt_cmd = f"sudo cryptsetup close '{self.map_name}'"
        run_cmd(cmd=decrypt_cmd)


@dataclass
class TemporaryMountDir:
    device: Path
    mount_dir: Optional[TemporaryDirectory] = None

    def __enter__(self) -> Path:
        self.mount_dir = TemporaryDirectory()
        run_cmd(
            cmd=f"sudo mount -o compress=zlib '{self.device}' '{self.mount_dir.name}'"
        )
        return Path(self.mount_dir.name)

    def __exit__(self, exc, value, tb):
        if self.mount_dir is None:
            return
        run_cmd(cmd=f"sudo umount '{self.mount_dir.name}'")
        self.mount_dir.__exit__(exc, value, tb)


@dataclass(frozen=True)
class RawButterConfig:
    uuid: str
    pass_cmd: str
    routes: list[tuple[str, str]]

    @classmethod
    def from_dict(cls, cfg: dict[str, Any]) -> RawButterConfig:
        expected_keys = {"UUID", "PassCmd", "Routes"}
        if expected_keys != set(cfg.keys()):
            sys.exit("Additional or missing keys in configuration.")
        if any(len(cur_route) != 2 for cur_route in cfg["Routes"]):
            sys.exit("All routes must have exactly 2 elements.")
        return cls(
            uuid=cfg["UUID"],
            pass_cmd=cfg["PassCmd"],
            routes=[(src, dest) for (src, dest) in cfg["Routes"]],
        )


@dataclass(frozen=True)
class ButterConfig:
    date: dt.date
    device: Path
    password: str
    routes: list[tuple[Path, str]]
    map_base: str = "butterbackup_"

    def map_name(self) -> str:
        return self.map_base + self.date.isoformat()

    def map_dir(self) -> Path:
        return Path("/dev/mapper/") / self.map_name()

    @classmethod
    def from_raw_config(cls, raw_cfg: RawButterConfig) -> ButterConfig:
        device = Path("/dev/disk/by-uuid") / raw_cfg.uuid
        pwd_process = run_cmd(cmd=raw_cfg.pass_cmd, capture_output=True)
        password = pwd_process.stdout.decode("utf-8").strip()
        routes = [(Path(src), dest) for (src, dest) in raw_cfg.routes]
        return cls(
            date=dt.date.today(),
            device=device,
            password=password,
            routes=routes,
        )


def main():
    cfg_file = parse_args()
    with cfg_file.open() as fh:
        config_lst = json.load(fh)
    if len(config_lst) == 0:
        sys.exit("Empty configurations are not allowed!\n")
    for cfg in config_lst:
        raw_cfg = RawButterConfig.from_dict(cfg)
        cfg = ButterConfig.from_raw_config(raw_cfg)
        do_butter_backup(cfg)


def parse_args() -> Path:
    parser = ArgumentParser()
    parser.add_argument(
        "--config", default=Path("~/.config/butter-backup.cfg").expanduser(), type=Path
    )
    args = parser.parse_args()
    cfg: Path = args.config
    return cfg


def do_butter_backup(cfg: ButterConfig) -> None:  # noqa: C901
    def get_source_snapshot(root: Path) -> Path:
        return max(root.glob("202?-*"))

    def snapshot(*, src: Path, dest: Path) -> None:
        run_cmd(cmd=f"echo btrfs subvolume snapshot '{src}' '{dest}'")

    def rsync(src, dest) -> None:
        run_cmd(cmd=f"echo rsync -ax --delete --inplace '{src}/' '{dest}'")

    with DecryptedDevice(cfg.device, cfg.map_name(), cfg.password) as decrypted:
        with TemporaryMountDir(decrypted) as mount_dir:
            backup_root = mount_dir / dt.date.today().isoformat()
            src_snapshot = get_source_snapshot(mount_dir)

            snapshot(src=src_snapshot, dest=backup_root)
            for src, dest_name in cfg.routes:
                dest = backup_root / dest_name
                rsync(src, dest)


def run_cmd(
    *, cmd: str, env: Optional[dict[str, str]] = None, capture_output: bool = False
) -> subprocess.CompletedProcess:
    if env is None:
        env = {}
    result = subprocess.run(
        cmd, capture_output=capture_output, check=True, env=env, shell=True
    )
    return result


if __name__ == "__main__":
    main()
