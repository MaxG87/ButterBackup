#!/usr/bin/env python3

from __future__ import annotations

import datetime as dt
import json
import os
import subprocess
import sys
from argparse import ArgumentParser
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any, Optional

DEFAULT_CONFIG_DIR = Path("~/.config/").expanduser()
DEFAULT_CONFIG_NAME = Path("butter-backup.cfg")
DEFAULT_CONFIG = DEFAULT_CONFIG_DIR / DEFAULT_CONFIG_NAME


@dataclass(frozen=True)
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
        if is_mounted(self.device):
            unmount_device(self.device)
        self.mount_dir = TemporaryDirectory()
        mount_btrfs_device(self.device, Path(self.mount_dir.name))
        return Path(self.mount_dir.name)

    def __exit__(self, exc, value, tb) -> None:
        if self.mount_dir is None:
            return
        unmount_device(self.device)
        self.mount_dir.__exit__(exc, value, tb)


@dataclass(frozen=True)
class ParsedButterConfig:
    files: set[str]
    files_dest: str
    folders: set[tuple[str, str]]
    pass_cmd: str
    uuid: str

    @classmethod
    def from_dict(cls, cfg: dict[str, Any]) -> ParsedButterConfig:
        expected_keys = {"UUID", "PassCmd", "Folders", "Files"}
        if expected_keys != set(cfg.keys()):
            sys.exit("Additional or missing keys in configuration.")
        if any(len(cur_route) != 2 for cur_route in cfg["Folders"]):
            sys.exit("All folder backup mappings must have exactly 2 elements.")
        return cls(
            uuid=cfg["UUID"],
            pass_cmd=cfg["PassCmd"],
            folders={(src, dest) for (src, dest) in cfg["Folders"]},
            files_dest=cfg["Files"]["destination"],
            files=set(cfg["Files"]["files"]),
        )


@dataclass(frozen=True)
class ButterConfig:
    date: dt.date
    device: Path
    files: set[Path]
    files_dest: str
    folders: set[tuple[Path, str]]
    pass_cmd: str
    map_base: str = "butterbackup_"

    def __post_init__(self) -> None:
        sources = Counter(src for (src, _) in self.folders)
        destinations = Counter(dest for (_, dest) in self.folders)
        file_names = Counter(f.name for f in self.files)
        self.exit_with_message_upon_duplicate(
            sources, ("Quellverzeichnissen", "Quellen")
        )
        self.exit_with_message_upon_duplicate(
            destinations, ("Zielverzeichnissen", "Ziele")
        )
        self.exit_with_message_upon_duplicate(file_names, ("Dateinamen", "Dateinamen"))
        self._ensure_all_folder_src_are_existing_dirs()
        self._ensure_all_file_src_are_existing_files()
        if self.files_dest in destinations:
            sys.exit(
                f"Zielverzeichnis {self.files_dest} ist gleichzeitig Ziel für Ordner und Einzeldateien."
            )

    @staticmethod
    def exit_with_message_upon_duplicate(
        counts: Counter, token: tuple[str, str]
    ) -> None:
        if all(val == 1 for val in counts.values()):
            return
        errmsg_begin = (
            f"Duplikate in {token[0]} entdeckt. Folgende {token[1]} kommen doppelt vor:"
        )
        errmsg_body = " ".join(
            str(elem) for (elem, count) in counts.items() if count > 1
        )
        sys.exit(f"{errmsg_begin} {errmsg_body}")

    def _ensure_all_folder_src_are_existing_dirs(self) -> None:
        uuid = self.device.name
        for src, _ in self.folders:
            if not src.exists():
                sys.exit(
                    f"Konfiguration für UUID {uuid} nennt nicht existierendes Quellverzeichnis {src}."
                )
            if not src.is_dir():
                sys.exit(
                    f"Konfiguration für UUID {uuid} enthält Quelle {src} die kein Verzeichnis ist."
                )

    def _ensure_all_file_src_are_existing_files(self) -> None:
        uuid = self.device.name
        for src in self.files:
            if not src.exists():
                sys.exit(
                    f"Konfiguration für UUID {uuid} nennt nicht existierende Quelldatei {src}."
                )
            if not src.is_file():
                sys.exit(
                    f"Konfiguration für UUID {uuid} enthält Quelle {src} die keine Datei ist."
                )

    def map_name(self) -> str:
        return self.map_base + self.date.isoformat()

    def map_dir(self) -> Path:
        return Path("/dev/mapper/") / self.map_name()

    @classmethod
    def from_raw_config(cls, raw_cfg: ParsedButterConfig) -> ButterConfig:
        device = Path("/dev/disk/by-uuid") / raw_cfg.uuid
        folders = {(Path(src).expanduser(), dest) for (src, dest) in raw_cfg.folders}
        files = {Path(src).expanduser() for src in raw_cfg.files}
        return cls(
            date=dt.date.today(),
            device=device,
            files=files,
            files_dest=raw_cfg.files_dest,
            folders=folders,
            pass_cmd=raw_cfg.pass_cmd,
        )


def main() -> None:
    cfg_file = parse_args()
    config_list = load_configuration(cfg_file)
    for raw_cfg in config_list:
        parsed_cfg = ParsedButterConfig.from_dict(raw_cfg)
        cfg = ButterConfig.from_raw_config(parsed_cfg)
        if cfg.device.exists():
            do_butter_backup(cfg)


def parse_args() -> Path:
    parser = ArgumentParser()
    parser.add_argument(
        "--config",
        default=os.getenv("XDG_CONFIG_HOME", DEFAULT_CONFIG_DIR) / DEFAULT_CONFIG_NAME,
        type=Path,
    )
    args = parser.parse_args()
    cfg: Path = args.config
    return cfg


def load_configuration(cfg_file: Path) -> list[dict[str, Any]]:
    if not cfg_file.exists():
        err_msg = f"Konfigurationsdatei {cfg_file} existiert nicht."
        help_hint = "Nutzen Sie `--help` um zu erfahren, wie eine Konfigurationsdatei explizit angegeben werden kann."
        sys.exit(f"{err_msg} {help_hint}\n")

    with cfg_file.open() as fh:
        config_lst = json.load(fh)
    if len(config_lst) == 0:
        sys.exit("Empty configurations are not allowed!\n")
    return config_lst  # type: ignore


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
            for src, dest_name in cfg.folders:
                dest = backup_root / dest_name
                rsync(src, dest)


def is_mounted(dest: Path) -> bool:
    return str(dest) in get_mounted_devices()


def get_mounted_devices() -> dict[str, Path]:
    raw_mounts = run_cmd(cmd="mount", capture_output=True)
    mount_lines = raw_mounts.stdout.decode().splitlines()
    return {line.split()[0]: Path(line.split()[2]) for line in mount_lines}


def mount_btrfs_device(device: Path, mount_dir: Path) -> None:
    run_cmd(cmd=f"sudo mount -o compress=zlib '{device}' '{mount_dir}'")


def unmount_device(device: Path) -> None:
    run_cmd(cmd=f"sudo umount '{device}'")


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
