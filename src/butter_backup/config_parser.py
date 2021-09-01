from __future__ import annotations

import datetime as dt
import json
import sys
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict
from uuid import UUID

RAW_CONFIG_T = Dict[str, Any]


@dataclass(frozen=True)
class ParsedButterConfig:
    files: set[str]
    files_dest: str
    folders: set[tuple[str, str]]
    pass_cmd: str
    uuid: UUID

    @classmethod
    def from_dict(cls, cfg: RAW_CONFIG_T) -> ParsedButterConfig:
        expected_keys = {"UUID", "PassCmd", "Folders", "Files"}
        if expected_keys != set(cfg.keys()):
            sys.exit("Additional or missing keys in configuration.")
        if any(len(cur_route) != 2 for cur_route in cfg["Folders"]):
            sys.exit("All folder backup mappings must have exactly 2 elements.")
        return cls(
            files=set(cfg["Files"]["files"]),
            files_dest=cfg["Files"]["destination"],
            folders={(src, dest) for (src, dest) in cfg["Folders"]},
            pass_cmd=cfg["PassCmd"],
            uuid=UUID(cfg["UUID"]),
        )

    def as_dict(self) -> RAW_CONFIG_T:
        return {
            "Files": {"destination": self.files_dest, "files": list(self.files)},
            "Folders": [[src, dest] for (src, dest) in self.folders],
            "PassCmd": self.pass_cmd,
            "UUID": str(self.uuid),
        }


@dataclass(frozen=True)
class ButterConfig:
    date: dt.date
    device: Path
    files: set[Path]
    files_dest: str
    folders: set[tuple[Path, str]]
    pass_cmd: str
    uuid: UUID
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
        for src, _ in self.folders:
            if not src.exists():
                sys.exit(
                    f"Konfiguration für UUID {self.uuid} nennt nicht existierendes Quellverzeichnis {src}."
                )
            if not src.is_dir():
                sys.exit(
                    f"Konfiguration für UUID {self.uuid} enthält Quelle {src} die kein Verzeichnis ist."
                )

    def _ensure_all_file_src_are_existing_files(self) -> None:
        for src in self.files:
            if not src.exists():
                sys.exit(
                    f"Konfiguration für UUID {self.uuid} nennt nicht existierende Quelldatei {src}."
                )
            if not src.is_file():
                sys.exit(
                    f"Konfiguration für UUID {self.uuid} enthält Quelle {src} die keine Datei ist."
                )

    def map_name(self) -> str:
        return self.map_base + self.date.isoformat()

    def map_dir(self) -> Path:
        return Path("/dev/mapper/") / self.map_name()

    @classmethod
    def from_raw_config(cls, raw_cfg: ParsedButterConfig) -> ButterConfig:
        device = Path("/dev/disk/by-uuid") / str(raw_cfg.uuid)
        folders = {(Path(src).expanduser(), dest) for (src, dest) in raw_cfg.folders}
        files = {Path(src).expanduser() for src in raw_cfg.files}
        return cls(
            date=dt.date.today(),
            device=device,
            files=files,
            files_dest=raw_cfg.files_dest,
            folders=folders,
            pass_cmd=raw_cfg.pass_cmd,
            uuid=raw_cfg.uuid,
        )


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
