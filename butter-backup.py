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
    pass_cmd: str

    def __enter__(self) -> Path:
        decrypt_cmd = f"sudo cryptsetup open '{self.device}' '{self.map_name}'"
        subprocess.run(f"{self.pass_cmd} | {decrypt_cmd}", check=True, shell=True)
        return Path(f"/dev/mapper/{self.map_name}")

    def __exit__(self, exc, value, tb) -> None:
        decrypt_cmd = f"sudo cryptsetup close '{self.map_name}'"
        run_cmd(cmd=decrypt_cmd)


@dataclass
class MountedDevice:
    device: Path
    mount_dir: Optional[TemporaryDirectory] = None

    def __enter__(self) -> Path:
        self.mount_dir = TemporaryDirectory()
        run_cmd(
            cmd=f"sudo mount -o compress=zlib '{self.device}' '{self.mount_dir.name}'"
        )
        return Path(self.mount_dir.name)

    def __exit__(self, exc, value, tb) -> None:
        if self.mount_dir is None:
            return
        run_cmd(cmd=f"sudo umount '{self.mount_dir.name}'")
        self.mount_dir.__exit__(exc, value, tb)


@dataclass(frozen=True)
class ParsedButterConfig:
    uuid: str
    pass_cmd: str
    routes: list[tuple[str, str]]

    @classmethod
    def from_dict(cls, cfg: dict[str, Any]) -> ParsedButterConfig:
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
    pass_cmd: str
    routes: list[tuple[Path, str]]
    map_base: str = "butterbackup_"

    def map_name(self) -> str:
        return self.map_base + self.date.isoformat()

    def map_dir(self) -> Path:
        return Path("/dev/mapper/") / self.map_name()

    @classmethod
    def from_raw_config(cls, raw_cfg: ParsedButterConfig) -> ButterConfig:
        device = Path("/dev/disk/by-uuid") / raw_cfg.uuid
        routes = [(Path(src).expanduser(), dest) for (src, dest) in raw_cfg.routes]
        return cls(
            date=dt.date.today(),
            device=device,
            pass_cmd=raw_cfg.pass_cmd,
            routes=routes,
        )


def main() -> None:
    cfg_file = parse_args()
    with cfg_file.open() as fh:
        config_lst = json.load(fh)
    if len(config_lst) == 0:
        sys.exit("Empty configurations are not allowed!\n")
    for raw_cfg in config_lst:
        parsed_cfg = ParsedButterConfig.from_dict(raw_cfg)
        cfg = ButterConfig.from_raw_config(parsed_cfg)
        do_butter_backup(cfg)


def parse_args() -> Path:
    parser = ArgumentParser()
    parser.add_argument(
        "--config", default=Path("~/.config/butter-backup.cfg").expanduser(), type=Path
    )
    args = parser.parse_args()
    cfg: Path = args.config
    return cfg


def do_butter_backup(cfg: ButterConfig) -> None:
    def get_source_snapshot(root: Path) -> Path:
        return max(root.glob("202?-*"))

    def snapshot(*, src: Path, dest: Path) -> None:
        run_cmd(cmd=f"sudo btrfs subvolume snapshot '{src}' '{dest}'")

    def rsync(src: Path, dest: Path) -> None:
        run_cmd(cmd=f"sudo rsync -ax --delete --inplace '{src}/' '{dest}'")

    with DecryptedDevice(cfg.device, cfg.map_name(), cfg.pass_cmd) as decrypted:
        with MountedDevice(decrypted) as mount_dir:
            backup_root = mount_dir / dt.datetime.now().strftime("%F_%H:%M:%S")
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
