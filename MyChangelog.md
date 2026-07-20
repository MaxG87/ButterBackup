This is a major release with many exciting new features and improvements.
Unfortunately, it couldn't be avoided to introduce some breaking changes, which
are listed below.

The most exciting new feature is the introduction of SudoPassCmd, which
allows users to specify a command that refreshes the sudo cache. This is
particularly useful for long-running backup processes, as it prevents them from
being interrupted due to sudo timeouts.

Almost as exciting is a rewrite of the device opening logic. Now users can
specify an `OpenDirectory` in their configuration file. If present, the program
will open the backup device in a subfolder of that directory. The subfolder
will be named after the device's `Name`. With that, opening paths become
predictable and former commonds from the shell history can be reused without
adapting to some random opening path. This new logic is applied to subcommands
`open` and `backup`.

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

There was extensive work on the tests and correctness. Due to this, several
annoying bugs were fixed. Most notably, closing devices whose `Name` contains a
space now works as expected. To drive this using tests, the test suite now runs
for three different device names.

Furthermore, in some cases tests were added for correct behaviour. In these
cases, the tested behaviour was considered important but its correctness was
not guaranteed. In some cases this improved.
