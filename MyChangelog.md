# Changelog for ButterBackup

## Release Highlights

This is a major release with several exciting new features and quite a few
breaking changes. I'm quite proud of what came out of it.

The star of the show is `SudoPassCmd`: a command that refreshes the sudo
cache, so long-running backups no longer get interrupted by sudo timeouts.
Almost as exciting is the rework of device opening: with the new
`OpenDirectory` option, devices always open into a predictable subfolder
named after the device, so old shell history actually keeps working.

Configuration handling also got a substantial overhaul. ButterBackup now has
a proper default config location under `$XDG_CONFIG_HOME`, and supports YAML,
JSON5, and TOML in addition to plain JSON. I switched my own configs to
JSON5 because I was tired of JSON not allowing trailing commas.

On the backup side, `format-device` now supports `--file-system` to set up
devices with Ext4 instead of Btrfs when using the Restic module. I experienced
corruption of Btrfs on undervolted external hard disks. There are reports that
Ext4 is more stable in this scenario, so I added the option. I'm not fully
convinced about this, but now it's your choice to make.

Finally, this release includes a lot of under-the-hood test and correctness
work, fixing several long-standing bugs along the way.

See below for the full list of changes, including breaking changes and a
migration example for the configuration format.

## Breaking Changes

- The configuration file format has changed: there is now a device-independent
  top-level section containing global configuration options. What used to be
  the top-level list of device configs is now nested under the key
  `DeviceConfigurations` within that section. Existing configuration files
  need to be migrated to the new structure.

  Before (plain JSON, top-level list of device configs):

```json
[
  {
    "Name": "BtrFS Backup Example",
    "UUID": "12345678-1234-5678-1234-567812345678",
    "DevicePassCmd": "echo device-password",
    "BackupRepositoryFolder": "ButterBackupRepo",
    "Compression": "zstd:3",
    "Folders": { "/tmp": "temp-files" },
    "Files": [],
    "FilesDest": "single-files"
  },
  {
    "Name": "Restic Backup Example",
    "UUID": "87654321-4321-8765-4321-876543218765",
    "DevicePassCmd": "echo device-password",
    "BackupRepositoryFolder": "ResticRepo",
    "RepositoryPassCmd": "echo repo-password",
    "FilesAndFolders": ["/tmp"]
  }
]
```

After (JSON5, global section with `DeviceConfigurations` key):

```json5
// JSON5 allows comments and trailing commas
{
  SudoPassCmd: "gpg --decrypt ~/.local/share/passwords/sudo-password.gpg",
  // OpenDirectory: "/media/ButterBackup",  // optional: where to open devices
  DeviceConfigurations: [
    {
      Name: "BtrFS Backup Example",
      UUID: "12345678-1234-5678-1234-567812345678",
      DevicePassCmd: "echo device-password",
      BackupRepositoryFolder: "ButterBackupRepo",
      Compression: "zstd:3",
      Folders: { "/tmp": "temp-files" },
      Files: [],
      FilesDest: "single-files",
    },
    {
      Name: "Restic Backup Example",
      UUID: "87654321-4321-8765-4321-876543218765",
      DevicePassCmd: "echo device-password",
      BackupRepositoryFolder: "ResticRepo",
      RepositoryPassCmd: "echo repo-password",
      FilesAndFolders: ["/tmp"],
    },
  ],
}
```

- The default configuration file location has moved to a project-specific
  subfolder under `$XDG_CONFIG_HOME`. Existing configs in the old location
  will need to be moved manually.

- Device opening paths have changed for the `open` and `backup` subcommands.

## Added

- `SudoPassCmd`: specify a command that refreshes the sudo cache, preventing
  long-running backups from being interrupted by sudo timeouts.
- `OpenDirectory` configuration option: backup devices are now opened in a
  subfolder (named after the device's `Name`) of a configurable directory,
  making opening paths predictable and reusable from shell history.
- Support for additional configuration file formats: YAML, JSON5, and TOML.
- `format-device` now accepts a `--file-system` option, allowing Ext4 as an
  alternative to Btrfs when using the Restic backup module. This may help avoid
  file system corruption issues on external hard disks.

## Improved reliability

- Significant work went into test coverage and correctness this release.
  Several long-standing bugs were fixed, including an issue where closing
  devices whose `Name` contains a space did not work correctly.
