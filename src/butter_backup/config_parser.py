from __future__ import annotations

import json
import sys
import uuid
from collections import Counter
from pathlib import Path
from typing import Any, Dict, Iterable, Union

from pydantic import (
    BaseModel,
    DirectoryPath,
    Extra,
    FilePath,
    root_validator,
    validator,
)

RAW_CONFIG_T = Dict[str, Any]
FoldersT = dict[DirectoryPath, str]


def path_aware_json_decoding(v, *, default) -> str:
    v["Folders"] = {str(key): val for key, val in v["Folders"].items()}
    return json.dumps(v, default=default)


class BtrfsConfig(BaseModel):
    Files: set[FilePath]
    FilesDest: str
    Folders: FoldersT
    PassCmd: str
    UUID: uuid.UUID

    class Config:
        extra = Extra.forbid
        frozen = True
        json_dumps = path_aware_json_decoding

    @validator("Files")
    def source_file_names_must_be_unique(cls, files):
        file_names = Counter(f.name for f in files)
        cls.raise_with_message_upon_duplicate(file_names, ("Dateinamen", "Dateinamen"))
        return files

    @validator("Folders", pre=True)
    def handle_legacy_folders_cfg(cls, folders):
        if isinstance(folders, dict):
            return folders

        sources = Counter(src for src, _ in folders)
        cls.raise_with_message_upon_duplicate(
            sources, ("Quellverzeichnissen", "Quellen")
        )

        as_dict = {src: dest for src, dest in folders}
        return as_dict

    @validator("Folders")
    def folder_destinations_must_be_unique(cls, folders: FoldersT) -> FoldersT:
        destinations = Counter(folders.values())
        cls.raise_with_message_upon_duplicate(
            destinations, ("Zielverzeichnissen", "Ziele")
        )
        return folders

    @validator("Folders", pre=True)
    def expand_tilde_in_folder_sources(cls, folders):
        new = {Path(src).expanduser(): dest for src, dest in folders.items()}
        return new

    @validator("Files", pre=True)
    def expand_tilde_in_file_sources(cls, files):
        new = [Path(cur).expanduser() for cur in files]
        return new

    @root_validator(pre=True)
    def handle_legacy_files_cfg(cls, values):
        try:
            files = values["Files"]["files"]
            files_dest = values["Files"]["destination"]
        except (KeyError, TypeError):
            return values
        values["Files"] = files
        values["FilesDest"] = files_dest
        return values

    @root_validator(skip_on_failure=True)
    def files_dest_is_no_folder_dest(cls, values):
        files_dest = values["FilesDest"]
        destinations = values["Folders"].values()
        if files_dest in destinations:
            raise ValueError(
                f"Zielverzeichnis {files_dest} ist gleichzeitig Ziel für Ordner und Einzeldateien."
            )
        return values

    @staticmethod
    def raise_with_message_upon_duplicate(
        counts: Union[Counter[Path], Counter[str]], token: tuple[str, str]
    ) -> None:
        if all(val == 1 for val in counts.values()):
            return
        errmsg_begin = (
            f"Duplikate in {token[0]} entdeckt. Folgende {token[1]} kommen doppelt vor:"
        )
        errmsg_body = " ".join(
            str(elem) for (elem, count) in counts.items() if count > 1
        )
        raise ValueError(f"{errmsg_begin} {errmsg_body}")

    def device(self) -> Path:
        return Path(f"/dev/disk/by-uuid/{self.UUID}")

    def map_name(self) -> str:
        return str(self.UUID)


class ResticConfig(BaseModel):
    DevicePassCmd: str
    Files: set[FilePath]
    FilesDest: str
    Folders: FoldersT
    ResticPassCmd: str
    UUID: uuid.UUID

    class Config:
        extra = Extra.forbid
        frozen = True
        json_dumps = path_aware_json_decoding

    @validator("Files")
    def source_file_names_must_be_unique(cls, files):
        file_names = Counter(f.name for f in files)
        cls.raise_with_message_upon_duplicate(file_names, ("Dateinamen", "Dateinamen"))
        return files

    @validator("Folders")
    def folder_destinations_must_be_unique(cls, folders: FoldersT) -> FoldersT:
        destinations = Counter(folders.values())
        cls.raise_with_message_upon_duplicate(
            destinations, ("Zielverzeichnissen", "Ziele")
        )
        return folders

    @validator("Folders", pre=True)
    def expand_tilde_in_folder_sources(cls, folders):
        new = {Path(src).expanduser(): dest for src, dest in folders.items()}
        return new

    @validator("Files", pre=True)
    def expand_tilde_in_file_sources(cls, files):
        new = [Path(cur).expanduser() for cur in files]
        return new

    @root_validator(skip_on_failure=True)
    def files_dest_is_no_folder_dest(cls, values):
        files_dest = values["FilesDest"]
        destinations = values["Folders"].values()
        if files_dest in destinations:
            raise ValueError(
                f"Zielverzeichnis {files_dest} ist gleichzeitig Ziel für Ordner und Einzeldateien."
            )
        return values

    @staticmethod
    def raise_with_message_upon_duplicate(
        counts: Union[Counter[Path], Counter[str]], token: tuple[str, str]
    ) -> None:
        if all(val == 1 for val in counts.values()):
            return
        errmsg_begin = (
            f"Duplikate in {token[0]} entdeckt. Folgende {token[1]} kommen doppelt vor:"
        )
        errmsg_body = " ".join(
            str(elem) for (elem, count) in counts.items() if count > 1
        )
        raise ValueError(f"{errmsg_begin} {errmsg_body}")

    def device(self) -> Path:
        return Path(f"/dev/disk/by-uuid/{self.UUID}")

    def map_name(self) -> str:
        return str(self.UUID)


def load_configuration(cfg_file: Path) -> Iterable[BtrfsConfig]:
    """Lade, parse und validiere die Konfigurationsdatei"""
    if not cfg_file.exists():
        err_msg = f"Konfigurationsdatei {cfg_file} existiert nicht."
        help_hint = "Nutzen Sie `--help` um zu erfahren, wie eine Konfigurationsdatei explizit angegeben werden kann."
        sys.exit(f"{err_msg} {help_hint}\n")

    config_lst = json.loads(cfg_file.read_text())
    if len(config_lst) == 0:
        sys.exit("Leere Konfigurationsdateien sind nicht erlaubt.\n")
    if not isinstance(config_lst, list):
        sys.exit("Die Konfiguration muss eine JSON-Liste sein!")
    if not all(isinstance(elem, dict) for elem in config_lst):
        sys.exit("Alle Einträge müssen ein JSON-Dictionary sein.")

    for raw_cfg in config_lst:
        # TODO Einige Validierungsfehler dürfen nicht zum Programmabbruch
        # führen. Ein Beispiel wären fehlende Dateien. Dadurch würde es
        # möglich, einen gemeinsaman Satz Konfigurationen für verschiedene
        # Rechner zu nutzen.
        yield BtrfsConfig.parse_obj(raw_cfg)
