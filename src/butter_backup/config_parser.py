import json
import sys
import uuid
from collections import Counter
from pathlib import Path
from typing import ClassVar

from pydantic import (
    BaseModel,
    ConfigDict,
    DirectoryPath,
    FilePath,
    TypeAdapter,
    field_validator,
    model_validator,
)
from storage_device_managers import ValidCompressions

FoldersT = dict[DirectoryPath, str]


def path_aware_btrfs_json_decoding(folders: FoldersT) -> str:
    as_dict = {str(key): val for key, val in folders.items()}
    return json.dumps(as_dict)


def path_aware_restic_json_decoding(
    files_and_folders: set[FilePath | DirectoryPath],
) -> str:
    as_dict = {str(cur) for cur in files_and_folders}
    return json.dumps(as_dict)


class BaseConfig(BaseModel):
    BackupRepositoryFolder: str
    DevicePassCmd: str
    ExcludePatternsFile: FilePath | None = None
    UUID: uuid.UUID
    Compression: ValidCompressions | None = None

    @field_validator("ExcludePatternsFile", mode="before")
    def expand_tilde_in_exclude_patterns_file_name(
        cls, maybe_exclude_patterns
    ) -> str | None:
        if maybe_exclude_patterns is None:
            return None
        return str(Path(maybe_exclude_patterns).expanduser())

    def device(self) -> Path:
        return Path(f"/dev/disk/by-uuid/{self.UUID}")

    def map_name(self) -> Path:
        return Path(f"/dev/mapper/{self.UUID}")


class BtrFSRsyncConfig(BaseConfig):
    model_config = ConfigDict(extra="forbid", frozen=True)
    Files: set[FilePath]
    FilesDest: str
    Folders: FoldersT
    SubvolTimestampFmt: ClassVar[str] = "%F_%H:%M:%S"

    @field_validator("Files")
    def source_file_names_must_be_unique(cls, files):
        file_names = Counter(f.name for f in files)
        cls.raise_with_message_upon_duplicate(file_names, ("Dateinamen", "Dateinamen"))
        return files

    @field_validator("Folders")
    def folder_destinations_must_be_unique(cls, folders: FoldersT) -> FoldersT:
        destinations = Counter(folders.values())
        cls.raise_with_message_upon_duplicate(
            destinations, ("Zielverzeichnissen", "Ziele")
        )
        return folders

    @field_validator("Files", mode="before")
    def expand_tilde_in_file_sources(cls, files) -> list[str]:
        new = [str(Path(cur).expanduser()) for cur in files]
        return new

    @field_validator("Folders", mode="before")
    def expand_tilde_in_folder_sources(cls, folders) -> dict[str, str]:
        new = {str(Path(src).expanduser()): dest for src, dest in folders.items()}
        return new

    @model_validator(mode="after")
    def files_dest_is_no_folder_dest(self):
        files_dest = self.FilesDest
        destinations = self.Folders.values()
        if files_dest in destinations:
            raise ValueError(
                f"Zielverzeichnis {files_dest} ist gleichzeitig Ziel für Ordner und Einzeldateien."
            )
        return self

    @staticmethod
    def raise_with_message_upon_duplicate(
        counts: Counter[Path] | Counter[str], token: tuple[str, str]
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


class ResticConfig(BaseConfig):
    model_config = ConfigDict(extra="forbid", frozen=True)
    FilesAndFolders: set[FilePath | DirectoryPath]
    RepositoryPassCmd: str

    @field_validator("FilesAndFolders", mode="before")
    def expand_tilde_in_sources(cls, files_and_folders) -> set[str]:
        new = {str(Path(src).expanduser()) for src in files_and_folders}
        return new


Configuration = BtrFSRsyncConfig | ResticConfig


def parse_configuration(content: str) -> list[Configuration]:
    ConfigList = TypeAdapter(list[Configuration])
    config_lst = ConfigList.validate_json(content)
    if len(config_lst) == 0:
        sys.exit("Leere Konfigurationsdateien sind nicht erlaubt.\n")
    return config_lst
