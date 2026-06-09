<img src="../logo.png" alt="ButterBackup-Logo" height="150px" align="right">

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

If no `--config` argument is passed, ButterBackup checks the following default
paths in order:

1. `~/.config/butter-backup/config.json5`
2. `~/.config/butter-backup/config.json`
3. `~/.config/butter-backup/config.toml`
4. `~/.config/butter-backup/config.yaml`

It is also possible to specify the path to the configuration file for each
command using the `--config` flag.

Each device configuration in the configuration file contains all the
information necessary to create backups for that target device. This includes
the specific files and directories to be backed up.

Examples of configuration files can be found in the `examples/` directory.
There is one file per supported format. Each example file contains all possible
fields, including optional ones, and one example for each module.

### Supported File Formats

ButterBackup accepts configuration files in the following formats:

- **JSON5** – JSON extension with comments and trailing commas (`*.json5`)
- **JSON** – Parsed as JSON5 (`*.json`)
- **TOML** – Standard TOML (`*.toml`)
- **YAML** – Standard YAML (`*.yaml`)

Note: Since `*.json` is parsed with the JSON5 parser, comments and trailing
commas are accepted there as well.

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

| Field       | Mandatory | Description                                                                          |
| ----------- | --------- | ------------------------------------------------------------------------------------ |
| `Folders`   | yes       | Mapping of source directories to destination directory names on the device           |
| `Files`     | yes       | Set of source files to be backed up individually                                     |
| `FilesDest` | yes       | Name of destination directory on the device to which the individual files are copied |

For the BtrFSRsync module, the following additional restrictions apply:

- The filenames of all entries in `Files` must be **unique**.
- The destination directory names in `Folders` must be **unique**.
- `FilesDest` must **not** match any destination directory name in `Folders`.

#### Fields specific to Restic

| Field               | Mandatory | Description                                                        |
| ------------------- | --------- | ------------------------------------------------------------------ |
| `FilesAndFolders`   | yes       | Set of source files and directories to be backed up                |
| `RepositoryPassCmd` | yes       | Shell command that produces the password for the Restic repository |

## Threat Scenarios

ButterBackup has been designed to protect against a range of specific threat
scenarios. The specific threats and the corresponding countermeasures are
listed in the table below. The summary table is followed by a slightly more
detailed discussion of the measures.

| Threat                                                     | Countermeasure                             |
| ---------------------------------------------------------- | ------------------------------------------ |
| File-encrypting malware                                    | Physically separate storage                |
| Faulty backup copies due to misuse                         | Very simple usage                          |
| Loss of the data storage medium                            | Full encryption of the data storage medium |
| Destruction of the data storage medium due to power surges | Physically separate storage                |
| Backups taken too rarely                                   | Very simple to use                         |

These threats are mitigated by the fact that the system is very simple to use,
enabling the physically separate storage of the data storage medium containing
the backup copies.

An easy-to-implement and reliable measure to protect the storage device from
damage caused by power surges is to store it physically separate from the
computer. At the same time, this measure also provides excellent protection
against the destruction of backup copies by malware, as the backup copies are
beyond the reach of the malware.

However, physically separate storage presents a threat due to convenience. A
process requiring manual steps is prone to errors and runs the risk of not
being carried out when in doubt. Creating backup copies is no exception here.

ButterBackup addresses this risk by reducing the creation of a backup copy to
just two manual steps. It suffices to connect the hard drive to the computer
and launch the programme. ButterBackup takes care of all further steps, such as
decryption, copying the data and unmounting.

Overall, ButterBackup enables you to store backup copies as securely as
possible without complicating the process of creating new backup copies.

## Running the tests

ButterBackup has a comprehensive test suite that covers many relevant aspects
of the programme. This is supplemented by the similarly comprehensive test
suites for the dependencies `storage-device-managers` and `shell-interface`.
These two dependencies originated from ButterBackup, meaning that their test
suites thoroughly cover further aspects that are important for ButterBackup.

For quick execution during development, it is advisable to run the test suite
directly. This is explained in the “quick” section. However, the
cross-distribution tests should also be run at the latest before a release, as
these guarantee that ButterBackup works on Arch and all supported Python
versions.

**⚠️ Please note:** The test suite requires and requests sudo privileges, as it
needs to create, encrypt, format and mount loop devices. However, the tests are
designed to be safe and do not make any permanent or significant changes to the
system. If you have any concerns in this regard, you can run the Docker-based
test suite instead.

### Quick execution

#### Direct

```bash
uv run ruff check .
uv run mypy .
uv run pytest
```

#### Using the Makefile

```bash
make check-format check-linters run-undockered-tests
```

### Cross-distribution tests

There is a comprehensive test suite that can also detect certain platform
dependencies. The Docker tests only exist because there were issues with Arch.

The test suite can be run using `make`. Parallelisation is also possible using
`make -j <N>`, where N specifies the number of processes. However, this was
found to be a bit unstable, so it is advised to resort to sequential execution
if you encounter any issues.

These tests are supplemented by a test matrix on GitHub. This test matrix
covers Ubuntu with all supported Python versions. This allows the very
long-running Docker test suite to be run less frequently where necessary.
