This is a major release with many exciting new features and improvements.
Unfortunately, it couldn't be avoided to introduce some breaking changes, which
are listed below.

The most exciting new feature is the introduction of SudoPassCmd, which
allows users to specify a command that refreshes the sudo cache. This is
particularly useful for long-running backup processes, as it prevents them from
being interrupted due to sudo timeouts.

Another amazing new feature is the added support for other configuration file
formats, including YAML, JSON5 and TOML. This allows users to choose the format
that best suits their needs and preferences. The author switched to use JSON5,
as he was annoyed by the lack of trailing commas in JSON.

In order to address file system corruption issues on external hard disks, the
format-device command now includes a --file-system option. This allows
users to set up a device using ext4 instead of btrfs when using the
restic backup module. Some claim that ext4 is more stable than btrfs.
While the author finds this claim questionable, the choice is now up to the
users.

There were more small improvements, internal and external, across the board,
which you can find in the full list of changes below.

