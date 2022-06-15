from __future__ import annotations

import json
import sys
import uuid
from collections import Counter
from pathlib import Path
from typing import ClassVar
from typing import Counter as CounterT
from typing import Dict, Iterable, Optional, Set, Union

from loguru import logger
from pydantic import (
    BaseModel,
    DirectoryPath,
    Extra,
    FilePath,
    ValidationError,
    root_validator,
    validator,
)

Configuration = Union["BtrfsConfig", "ResticConfig"]
FoldersT = Dict[DirectoryPath, str]


def path_aware_btrfs_json_decoding(v, *, default) -> str:
    v["Folders"] = {str(key): val for key, val in v["Folders"].items()}
    return json.dumps(v, default=default)


def path_aware_restic_json_decoding(v, *, default) -> str:
    v["FilesAndFolders"] = {str(cur) for cur in v["FilesAndFolders"]}
    return json.dumps(v, default=default)


class BtrfsConfig(BaseModel):
    BackupRepositoryFolder: str
    DevicePassCmd: str
    ExcludePatternsFile: Optional[FilePath] = None
    Files: Set[FilePath]
    FilesDest: str
    Folders: FoldersT
    UUID: uuid.UUID
    SubvolTimestampFmt: ClassVar[str] = "%F_%H:%M:%S"

    class Config:
        extra = Extra.forbid
        frozen = True
        json_dumps = path_aware_btrfs_json_decoding

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

    @validator("ExcludePatternsFile", pre=True)
    def expand_tilde_in_exclude_patterns_file_name(cls, maybe_exclude_patterns):
        if maybe_exclude_patterns is None:
            return None
        return Path(maybe_exclude_patterns).expanduser()

    @validator("Files", pre=True)
    def expand_tilde_in_file_sources(cls, files):
        new = [Path(cur).expanduser() for cur in files]
        return new

    @validator("Folders", pre=True)
    def expand_tilde_in_folder_sources(cls, folders):
        new = {Path(src).expanduser(): dest for src, dest in folders.items()}
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
        counts: Union[CounterT[Path], CounterT[str]], token: tuple[str, str]
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
    BackupRepositoryFolder: str
    DevicePassCmd: str
    ExcludePatternsFile: Optional[FilePath] = None
    FilesAndFolders: Set[Union[FilePath, DirectoryPath]]
    RepositoryPassCmd: str
    UUID: uuid.UUID

    class Config:
        extra = Extra.forbid
        frozen = True
        json_dumps = path_aware_restic_json_decoding

    @validator("ExcludePatternsFile", pre=True)
    def expand_tilde_in_exclude_patterns_file_name(cls, maybe_exclude_patterns):
        if maybe_exclude_patterns is None:
            return None
        return Path(maybe_exclude_patterns).expanduser()

    @validator("FilesAndFolders", pre=True)
    def expand_tilde_in_sources(cls, files_and_folders):
        new = {Path(src).expanduser() for src in files_and_folders}
        return new

    def device(self) -> Path:
        return Path(f"/dev/disk/by-uuid/{self.UUID}")

    def map_name(self) -> str:
        return str(self.UUID)


def load_configuration(cfg_file: Path) -> Iterable[Union[BtrfsConfig, ResticConfig]]:
    """Lade, parse und validiere die Konfigurationsdatei"""
    if not cfg_file.exists():
        err_msg = f"Konfigurationsdatei {cfg_file} existiert nicht."
        help_hint = "Nutzen Sie `--help` um zu erfahren, wie eine Konfigurationsdatei explizit angegeben werden kann."
        sys.exit(f"{err_msg} {help_hint}\n")

    config_lst = json.loads(cfg_file.read_text())
    ensure_valid_config_json_list(config_lst)
    logger.success(f"Konfigurationsdatei {cfg_file} erfolgreich eingelesen.")
    logger.info(f"Konfigurationsdatei {cfg_file} enthält {len(config_lst)} Einträge.")

    for raw_cfg in config_lst:
        # TODO Einige Validierungsfehler dürfen nicht zum Programmabbruch
        # führen. Ein Beispiel wären fehlende Dateien. Dadurch würde es
        # möglich, einen gemeinsaman Satz Konfigurationen für verschiedene
        # Rechner zu nutzen.
        config: Configuration
        try:
            type_ = "BtrFS"
            config = BtrfsConfig.parse_obj(raw_cfg)
        except ValidationError:
            type_ = "Restic"
            config = ResticConfig.parse_obj(raw_cfg)
        logger.success(
            f"{type_}-Konfiguration für UUID {config.UUID} erfolgreich geparst."
        )
        yield config


def ensure_valid_config_json_list(config_lst):
    # Getestet durch Tests für `load_configuration`.
    if len(config_lst) == 0:
        sys.exit("Leere Konfigurationsdateien sind nicht erlaubt.\n")
    if not isinstance(config_lst, list):
        sys.exit("Die Konfiguration muss eine JSON-Liste sein!")
    if not all(isinstance(elem, dict) for elem in config_lst):
        sys.exit("Alle Einträge müssen ein JSON-Dictionary sein.")
