# Changelog

## [3.5.0](https://github.com/MaxG87/ButterBackup/compare/v2.0.0...v3.5.0) (2026-07-22)


### ⚠ BREAKING CHANGES

* The libraries storage-device-managers and shell-interface now don't log any output. Code that configured them accordingly, or depended on that, will break.

### Features

* Add destination parameter to mounted_device ([33b6738](https://github.com/MaxG87/ButterBackup/commit/33b6738a82276fab03259499b43de6225049acf1))
* add msgspec dependency ([7c6dcbd](https://github.com/MaxG87/ButterBackup/commit/7c6dcbd006b8b2640d98d9805f25b28fe4ed92bb))
* Create a missing destination ([f09ec5f](https://github.com/MaxG87/ButterBackup/commit/f09ec5f5f4f6b31f7a3589c9c1fa4329f0c44e31))
* Return first created directory ancestor ([6ab58eb](https://github.com/MaxG87/ButterBackup/commit/6ab58ebfac9a783d3d654fb84acdca07ad08a17a))
* **sdm:** Add ensure_directory for privileged mkdir ([f32d38f](https://github.com/MaxG87/ButterBackup/commit/f32d38fe6aed676569f423c90533fcbecdb49797))


### Bug Fixes

* rewrite get_mounted_devices using findmnt --json + msgspec ([2954d7e](https://github.com/MaxG87/ButterBackup/commit/2954d7eddc4508661b3162dcf3ceaf4720f9ce02))
* **test:** Handle loopback devices to make test suite pass again ([c3126bc](https://github.com/MaxG87/ButterBackup/commit/c3126bc66d3a4c7cedd811eb63b2262fdafb9987))


### Dependencies

* Bump all prod and dev dependencies ([761d3e5](https://github.com/MaxG87/ButterBackup/commit/761d3e5a73ae26eabc04d5b5384726dfa4d72177))
* **dev:** bump hypothesis in the development-dependencies group ([#128](https://github.com/MaxG87/ButterBackup/issues/128)) ([799133a](https://github.com/MaxG87/ButterBackup/commit/799133ac77ab54e4cd215acc6ce916624f6158ab))


### Miscellaneous Chores

* release 3.5.0 ([9296019](https://github.com/MaxG87/ButterBackup/commit/92960193a649f4f4342c428e7a28ec1425c60f75))
* release v0.15.0 ([bd5bfcc](https://github.com/MaxG87/ButterBackup/commit/bd5bfcc8238268cc7c8d3c69af47f37fed175020))


### Code Refactoring

* Drop loguru from both libraries ([bdacc96](https://github.com/MaxG87/ButterBackup/commit/bdacc960d1a94b121f8401b1ed57f17ee5a3c241))

## [2.0.0](https://github.com/MaxG87/storage-device-managers/compare/v1.0.2...v2.0.0) (2026-04-27)


### ⚠ BREAKING CHANGES

* Drop support for Python 3.10

### Features

* add `__all__` to define the public API ([ce82161](https://github.com/MaxG87/storage-device-managers/commit/ce82161d62813d40864a431c272f8603cb66561a))
* add `get_filesystem` helper function ([06cde0c](https://github.com/MaxG87/storage-device-managers/commit/06cde0c91155f3201130c477446154e09c5d3d5f))
* Add `mkfs` function to format a device to any file system ([a313425](https://github.com/MaxG87/storage-device-managers/commit/a313425bc3269a6abd674bc8b7a716d3f5f771f2))
* add `mkfs_ext4` dedicated function ([6fa7efb](https://github.com/MaxG87/storage-device-managers/commit/6fa7efb2f3b348d4b1ac7a534313424e475e9874))
* add `mount_device` helper function for auto-detected filesystem ([6e43fed](https://github.com/MaxG87/storage-device-managers/commit/6e43fedd5c40a337c84bd383f7dba1e2ae46b7ce))
* add `mount_ext4_device` dedicated function ([d52db30](https://github.com/MaxG87/storage-device-managers/commit/d52db30fe1960d717a3c792c8f4596a0e4f719e8))
* bump shell-interface to `>=2.0.0` and expose `PassCmdError` ([6f978c0](https://github.com/MaxG87/storage-device-managers/commit/6f978c0ee7a6a9a3510c83461700e9e375061439))
* call filesystem sync on unmount ([1640b2e](https://github.com/MaxG87/storage-device-managers/commit/1640b2e1977e2af0e5a2b068c23051b5a418eedb))
* First run BtrFS syncs, then the device's one ([1436c1a](https://github.com/MaxG87/storage-device-managers/commit/1436c1a7f49eade3f79a8e29253503b7d9f6015a))
* modernise code for Python 3.11 minimum ([bf7ad02](https://github.com/MaxG87/storage-device-managers/commit/bf7ad0213011ee1d880cc82812d8fa63f56a6067))


### Bug Fixes

* rename temoprary_directory to temporary_directory ([8d8d164](https://github.com/MaxG87/storage-device-managers/commit/8d8d164a6da85325c75c04da08a45ab5356d33f8))


### Dependencies

* **dev:** Add pytest-xdist for faster execution ([95bdeac](https://github.com/MaxG87/storage-device-managers/commit/95bdeacd9f667e29a8797d1f034e259698cbf564))
* Drop support for Python 3.10 ([c0588f8](https://github.com/MaxG87/storage-device-managers/commit/c0588f88d4ae4de507eae5da1924df30355559f3))


### Documentation

* Also mention ShellInterfaceError alongside PassCmdError ([7d8bf61](https://github.com/MaxG87/storage-device-managers/commit/7d8bf61bf307fa8e1382015de2d67433f8198241))
* Improve documentation of get_mounted_device ([6f88bb0](https://github.com/MaxG87/storage-device-managers/commit/6f88bb04618b8738bc354be79a5a9f33f5e480da))
* replace black/isort badges with ruff badge ([e4b33a4](https://github.com/MaxG87/storage-device-managers/commit/e4b33a4bcec3db03fde8c07b31e5fc0ac86347a0))
* Update README for new additions ([b204986](https://github.com/MaxG87/storage-device-managers/commit/b204986b3dde58e2c3c138ebc5e60bbbefe43baf))
* use modern union type syntax in API reference ([002088b](https://github.com/MaxG87/storage-device-managers/commit/002088b17fd150aafa73cee67b218dca753e51eb))

## [1.0.2](https://github.com/MaxG87/storage-device-managers/compare/v1.0.1...v1.0.2) (2026-04-15)

### Bug Fixes

- **build:** Fix lock file ([1627e18](https://github.com/MaxG87/storage-device-managers/commit/1627e18c03d55f02188162f7440b70fdf3ed2da3))
- use `if recursive:` instead of `if recursive is not None:` in chown ([bdc7633](https://github.com/MaxG87/storage-device-managers/commit/bdc763333817656099d92dc600e680b93b372058))

## [1.0.1](https://github.com/MaxG87/storage-device-managers/compare/v1.0.0...v1.0.1) (2026-03-22)

### Bug Fixes

- Fix dramatic bug of deleting all content after failed unmount ([b53c5ff](https://github.com/MaxG87/storage-device-managers/commit/b53c5ffe3fdf44f8eb7867143365f2ce3e890880))

### Dependencies

- Bump minimum Python version to 3.10 ([6ca9e82](https://github.com/MaxG87/storage-device-managers/commit/6ca9e820373a64b96b68ece48f35332c829d71b7))
- Bump shell-interface to v1.0.2 ([2917e21](https://github.com/MaxG87/storage-device-managers/commit/2917e2130ee19e5bf3320f2fd810d81d8e10b80f))
- **dev:** Bump all dev-dependencies ([949af96](https://github.com/MaxG87/storage-device-managers/commit/949af96cbeb7eb25105c809807f9899f23fc4de9))
- **dev:** Bump all development dependencies ([949c53c](https://github.com/MaxG87/storage-device-managers/commit/949c53c7c74f7f9a03c846a58a9842a98b133eac))
- **dev:** Drop two unneeded requirements ([eb55455](https://github.com/MaxG87/storage-device-managers/commit/eb554558c6c558518c9a38ca82894e25384957d1))

### Documentation

- Minor formatting changes ([f32e26c](https://github.com/MaxG87/storage-device-managers/commit/f32e26cace4ee4abcf7dbb35e9ee792bbd676e00))

## [1.0.0](https://github.com/MaxG87/storage-device-managers/compare/v0.15.1...v1.0.0) (2025-02-15)

### Bug Fixes

- **ci:** Run publish jobs sequentially ([3aae7b3](https://github.com/MaxG87/storage-device-managers/commit/3aae7b368afd0cc082c146018df3e3d0203dbff3))

### Documentation

- Add some metadata URLs ([dd328f3](https://github.com/MaxG87/storage-device-managers/commit/dd328f3960e13c5fc857bb2ca35e7622eeb0ef9e))

## [0.15.1](https://github.com/MaxG87/storage-device-managers/compare/v0.15.0...v0.15.1) (2025-02-15)

### Dependencies

- Remove upper limits on all dependencies ([0237b95](https://github.com/MaxG87/storage-device-managers/commit/0237b952234e8cdba0b77e1228dc508584ea965f))

### Documentation

- Add generated README ([c0106c3](https://github.com/MaxG87/storage-device-managers/commit/c0106c38b01b378d606a58e3e4def030e2da4632))

## [0.15.0](https://github.com/MaxG87/storage-device-managers/compare/v0.14.0...v0.15.0) (2024-11-26)

### Features

- Include options in return value of `get_mounted_devices()` ([cfebce5](https://github.com/MaxG87/storage-device-managers/commit/cfebce50ff802adaf34cba3320f16dc81dcc05a8))

## [0.14.0](https://github.com/MaxG87/storage-device-managers/compare/v0.13.0...v0.14.0) (2023-12-22)

### Features

- Extend supported Python versions to 3.12 ([fc1caec](https://github.com/MaxG87/storage-device-managers/commit/fc1caecce945814e574ed33e6a6a64941546d5ce))

### Dependencies

- Bump all dev-dependencies ([ec00905](https://github.com/MaxG87/storage-device-managers/commit/ec00905f2cf326a69129bd7ab38b8b3c2425ea26))
- Bump all production dependencies ([0574e6d](https://github.com/MaxG87/storage-device-managers/commit/0574e6d698b71ae994627c79aceb91b1de794b3a))
- Bump shell-interface to 0.13.0 ([a5a4511](https://github.com/MaxG87/storage-device-managers/commit/a5a451179c08fc270fed58850a1198c66feb38e1))
