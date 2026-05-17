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
