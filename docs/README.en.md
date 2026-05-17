<img src="logo.png" alt="ButterBackup-Logo" height="150px" align="right">

# ButterBackup - Backups As Smooth As Butter!

**Incremental full backups thanks to BtrFS**

## Installation

ButterBackup can be installed directly from PyPI using `pipx`:

```bash
pipx install butter-backup
```

However, it is also possible to install the project from the local source code:

```bash
uv build
pipx install dist/butter-backup-<version>.whl
```

## Usage

[![asciicast](https://asciinema.org/a/HTFzRxEWw8ltCoP6NDNo6tITP.svg)](https://asciinema.org/a/HTFzRxEWw8ltCoP6NDNo6tITP)

## Available Modules for Creating Backup Copies

Currently, there are two modules available for creating backup copies. No
additional modules are planned at this time, but they are possible in
principle. Any inquiries on this topic are highly appreciated!

### BtrFSRsync

This module is particularly well-suited for users who want to easily browse
their backups in a file manager. It combines the advantages of BtrFS snapshots
with the efficiency of `rsync`. Each backup operation creates a new BtrFS
subvolume by taking a snapshot of the most recent subvolume. Then, `rsync` is
used to create an incremental copy of the source files into the new subvolume.
This has the advantage that the backup can be created very quickly, since only
the changed files need to be copied. At the same time, the backups are very
space-efficient, since only the changed blocks require additional storage
space.

Ultimately, every backup acts like a full backup, since each subvolume contains
all the files. At the same time, however, only the storage space required for
incremental backups is used.

The backups are very easy to browse, as each subvolume appears as a regular
directory. After running `butter-backup open`, all backup copies are
immediately available in the file manager.

**Note:** Due to the specific way `rsync` is used, files that have the same
name, size, and timestamp are not copied. Therefore, if, for example, a process
on a filesystem without modification timestamps repeatedly overwrites files
without changing their size, these files will not be updated in the backup.

### Restic

This module uses `restic` to create backups. `restic` is a highly sophisticated
program for creating encrypted backups with advanced deduplication. More
information about `restic` and its capabilities can be found on [the official
website](https://restic.net/). This module is recommended for technically savvy
users.

After running `butter-backup open`, only a single directory is available,
containing all `restic` backups. Individual backups can be accessed via the
`restic` command line.

## Configuration Files

ButterBackup allows and requires a separate configuration for each device, e.g.
an external hard drive. These configurations are stored in a single file, which
is read by ButterBackup.

ButterBackup expects the configuration file to be located at
`~/.config/butter-backup.cfg`, regardless of its format. However, it is
possible to specify the path to the configuration file for each command using
the `--config` flag.

Each device configuration in the configuration file contains all the
information necessary to create backups for that target device. This includes
the specific files and directories to be backed up.

Examples of configuration files can be found in the `examples/` directory.
There is one file per supported format. Each example file contains all possible
fields, including optional ones, and one example for each module.

### Supported File Formats

ButterBackup accepts configuration files in the following formats:

- **JSON** – Standard JSON
- **JSON5** – JSON extension with comments and trailing commas
- **TOML** – Standard TOML
- **YAML** – Standard YAML

Sample configurations for all four formats can be found in the `examples/` directory.

### Structure and Restrictions

A configuration file contains a **non-empty list** of device configurations.
Each configuration describes exactly one backup device and belongs to one of
the two available modules (BtrFSRsync or Restic).

The following restrictions apply to the entire list:

- The list must **not be empty**.
- The `Name` field must be **unique** within the list.

#### Common Fields for All Device Configurations

All device configurations share the following fields:

| Field                    | Mandatory | Description                                                                                            |
| ------------------------ | --------- | ------------------------------------------------------------------------------------------------------ |
| `UUID`                   | yes       | The UUID of the backup device (can be determined using the command `sudo blkid -s UUID /dev/<device>`) |
| `DevicePassCmd`          | yes       | Shell command that displays the password for device encryption                                         |
| `BackupRepositoryFolder` | yes       | Name of the directory on the device to which the backup is being made                                  |
| `Name`                   | no        | Display name of the configuration; must be a valid path component; is set to `UUID` if not specified   |
| `Compression`            | no        | desired BtrFS compression (e.g. `zstd:3`, defaults to none)                                            |
| `ExcludePatternsFile`    | no        | Path to a file with exclusion patterns (`rsync` format)                                                |

#### Fields specific to BtrFSRsync

| Feld        | Pflichtfeld | Beschreibung                                                                         |
| ----------- | ----------- | ------------------------------------------------------------------------------------ |
| `Folders`   | yes         | Mapping of source directories to destination directory names on the device           |
| `Files`     | yes         | Number of source files to be backed up individually                                  |
| `FilesDest` | yes         | Name of destination directory on the device to which the individual files are copied |

For the BtrFSRsync module, the following additional restrictions apply:

- The filenames of all entries in `Files` must be **unique**.
- The destination directory names in `Folders` must be **unique**.
- `FilesDest` must **not** match any destination directory name in `Folders`.

#### Fields specific to Restic

| Feld                | Pflichtfeld | Beschreibung                                                       |
| ------------------- | ----------- | ------------------------------------------------------------------ |
| `FilesAndFolders`   | yes         | Set of source files and directories to be backed up                |
| `RepositoryPassCmd` | yes         | Shell command that produces the password for the Restic repository |
