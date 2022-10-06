from __future__ import annotations

import enum
import json
import sys
import uuid
from collections import Counter
from pathlib import Path
from typing import ClassVar, Dict, List, Optional, Set, Union

from pydantic import (
    BaseModel,
    DirectoryPath,
    Extra,
    FilePath,
    parse_raw_as,
    root_validator,
    validator,
)

FoldersT = Dict[DirectoryPath, str]


def path_aware_btrfs_json_decoding(v, *, default) -> str:
    v["Folders"] = {str(key): val for key, val in v["Folders"].items()}
    return json.dumps(v, default=default)


def path_aware_restic_json_decoding(v, *, default) -> str:
    v["FilesAndFolders"] = {str(cur) for cur in v["FilesAndFolders"]}
    return json.dumps(v, default=default)


class ValidCompressions(enum.Enum):
    LZO = "lzo"
    ZLIB = "zlib"
    ZLIB1 = "zlib:1"
    ZLIB2 = "zlib:2"
    ZLIB3 = "zlib:3"
    ZLIB4 = "zlib:4"
    ZLIB5 = "zlib:5"
    ZLIB6 = "zlib:6"
    ZLIB7 = "zlib:7"
    ZLIB8 = "zlib:8"
    ZLIB9 = "zlib:9"
    ZSTD = "zstd"
    ZSTD1 = "zstd:1"
    ZSTD2 = "zstd:2"
    ZSTD3 = "zstd:3"
    ZSTD4 = "zstd:4"
    ZSTD5 = "zstd:5"
    ZSTD6 = "zstd:6"
    ZSTD7 = "zstd:7"
    ZSTD8 = "zstd:8"
    ZSTD9 = "zstd:9"
    ZSTD10 = "zstd:10"
    ZSTD11 = "zstd:11"
    ZSTD12 = "zstd:12"
    ZSTD13 = "zstd:13"
    ZSTD14 = "zstd:14"
    ZSTD15 = "zstd:15"


class BaseConfig(BaseModel):
    BackupRepositoryFolder: str
    DevicePassCmd: str
    ExcludePatternsFile: Optional[FilePath] = None
    UUID: uuid.UUID
    Compression: Optional[ValidCompressions] = None


class BtrFSRsyncConfig(BaseConfig):
    Files: Set[FilePath]
    FilesDest: str
    Folders: FoldersT
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


class ResticConfig(BaseConfig):
    FilesAndFolders: Set[Union[FilePath, DirectoryPath]]
    RepositoryPassCmd: str

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


Configuration = Union[BtrFSRsyncConfig, ResticConfig]


def parse_configuration(content: str) -> list[Configuration]:
    config_lst = parse_raw_as(List[Configuration], content)
    if len(config_lst) == 0:
        sys.exit("Leere Konfigurationsdateien sind nicht erlaubt.\n")
    return config_lst
