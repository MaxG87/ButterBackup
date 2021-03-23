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
            date = dt.date.today(),
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
    def decrypt(cfg: ButterConfig) -> None:
        run_cmd(cmd=f"echo cryptsetup luksOpen '{cfg.device}' '{cfg.map_name()}'")
        subprocess.run("cat", check=True, input=cfg.password.encode(), shell=True)

    def mount(cfg: ButterConfig, mount_dir: Path) -> None:
        run_cmd(cmd=f"echo mount -o compress=zlib '{cfg.map_dir()}' '{mount_dir}'")

    def get_source_snapshot(root: Path) -> Path:
        return max(root.glob("202?-*"))

    def snapshot(*, src: Path, dest: Path) -> None:
        run_cmd(cmd=f"btrfs subvolume snapshot '{src}' '{dest}'")


    def rsync(src, dest) -> None:
        r"""
        grep -v '^\s*#' "$ordnerliste" | while read -r line
        do
            orig=$(echo "$line" | cut -d ' ' -f1)/ # beachte abschließendes "/"!
            ziel=$(echo "$line" | cut -d ' ' -f2)
            curBackup="${backup_root}/${ziel}"
            rsync -ax --delete --inplace "$orig" "$curBackup"
        done
        """
        print(src, dest)

    decrypt(cfg)
    with TemporaryDirectory() as mount_dir_name:
        mount_dir = Path(mount_dir_name)
        mount(cfg, mount_dir)
        src_snapshot = get_source_snapshot(mount_dir)
        backup_root=mount_dir / dt.date.today().isoformat()
        snapshot(src=src_snapshot, dest=backup_root)
        for src, dest in cfg.routes:
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
