# Changelog

## [3.7.0](https://github.com/MaxG87/ButterBackup/compare/v3.6.0...v3.7.0) (2026-04-22)


### Features

* Add --file-system option to format-device CLI command ([442ecc4](https://github.com/MaxG87/ButterBackup/commit/442ecc4b4dda8db1dd5b4ad62dd440cb8e339a02))
* Allow btrfs file system argument for BtrFS-Rsync too ([f6ff28c](https://github.com/MaxG87/ButterBackup/commit/f6ff28c398527a9b12a12cd5cda68e56d2df186c))
* Change default file system for restic to ext4 ([6f504c5](https://github.com/MaxG87/ButterBackup/commit/6f504c5892f1e08efdb251bd684ae35026494041))


### Dependencies

* Bump all dependencies - prod and dev ([f6a03b2](https://github.com/MaxG87/ButterBackup/commit/f6a03b2a1903c659bfd1a365d94e2b124da5a1ab))

## [3.6.0](https://github.com/MaxG87/ButterBackup/compare/v3.5.5...v3.6.0) (2026-04-15)


### Features

* Add name attribute to ButterBackup configuration ([239af34](https://github.com/MaxG87/ButterBackup/commit/239af34d0c1fa94d3fb9a21a01089e6ef11304d7))
* Log that chown is about to take place ([5d44acc](https://github.com/MaxG87/ButterBackup/commit/5d44acc517e2310acf55332571fcc882efe7d49f))


### Bug Fixes

* Avoid changing ownership recursively in BtrFS backend ([77603df](https://github.com/MaxG87/ButterBackup/commit/77603df8021a3fbbf2d2da31b62ef09dd0ae9d90))
* **build:** Rebuild lockfile ([daaee11](https://github.com/MaxG87/ButterBackup/commit/daaee1197c578af507639b5d02e0eabea95ec95f))
* Fix spurious unmount errors by synchronising changes first ([00ac2a3](https://github.com/MaxG87/ButterBackup/commit/00ac2a36762f01c23723c13031f9f0bac3fbc5da))


### Documentation

* Add name entry to example configuration file ([15f59b2](https://github.com/MaxG87/ButterBackup/commit/15f59b2287e74be6a9aa98822335cb0be8bf8603))

## [3.5.5](https://github.com/MaxG87/ButterBackup/compare/v3.5.4...v3.5.5) (2026-03-24)


### Bug Fixes

* Fix backup deleting bug by bumping storage_device_managers to v1.0.1 ([a46acb4](https://github.com/MaxG87/ButterBackup/commit/a46acb465155b92820af1271822b302fd54a82c0))


### Dependencies

* **dev:** Add pytest-mock ([d1a4960](https://github.com/MaxG87/ButterBackup/commit/d1a4960b5243ddfd98c3e1ff6977ea7af3d9b207))
* **dev:** Bump all dev-dependencies ([a74a8d1](https://github.com/MaxG87/ButterBackup/commit/a74a8d165adb7dc492670c471d518f872ae9ab1c))


### Documentation

* Mention use of 1Password ([062172d](https://github.com/MaxG87/ButterBackup/commit/062172dc024ef5a4de6bd60d6a1151f7f0d28d3b))

## [3.5.4](https://github.com/MaxG87/ButterBackup/compare/v3.5.3...v3.5.4) (2026-01-10)


### Bug Fixes

* Change ownership of new snapshot to current user ([7d4e54e](https://github.com/MaxG87/ButterBackup/commit/7d4e54e0b80492e385b27a00051cea8dcaa0fa82))
* Fix uv.lock ([1fa59e2](https://github.com/MaxG87/ButterBackup/commit/1fa59e2cabe94de6f63401085cabe8c34e056ea7))


### Dependencies

* Bump all dependencies ([8496afc](https://github.com/MaxG87/ButterBackup/commit/8496afc46674231f68e4551a6120177964bbf02c))
* **dev:** Bump all dev-dependencies ([dfba38a](https://github.com/MaxG87/ButterBackup/commit/dfba38a5f9f3a9afb847896883708dd4ef75242a))


### Documentation

* Update Python versions in one comment ([f8dcc42](https://github.com/MaxG87/ButterBackup/commit/f8dcc42fdbbae301f0128e0bc8282fbbb6808b46))

## [3.5.3](https://github.com/MaxG87/ButterBackup/compare/v3.5.2...v3.5.3) (2025-09-19)


### Bug Fixes

* Fix behaviour of single files backup ([b8b46af](https://github.com/MaxG87/ButterBackup/commit/b8b46af8b5eea38f0dcda892cc27b65d4e42e5d8))
* Make collection of single files a set ([af0f3ab](https://github.com/MaxG87/ButterBackup/commit/af0f3ab33c07972aecc9c98dfad6d87a19b92d01))
* **tests:** Ändere Größe der Einzeldatei-Zufallsdatei ([26aa5c7](https://github.com/MaxG87/ButterBackup/commit/26aa5c736795f720b2b4c971a2adf933a21322f6))


### Documentation

* Add documentation of backup modules ([5f02eb3](https://github.com/MaxG87/ButterBackup/commit/5f02eb33b0fae177a418675757ff4a96d0c67945))
* Kleinere Verbesserung der Installationsanleitung ([962507a](https://github.com/MaxG87/ButterBackup/commit/962507a1b55cb2bd90812f057290938a8679df88))

## [3.5.2](https://github.com/MaxG87/ButterBackup/compare/v3.5.1...v3.5.2) (2025-07-03)


### Bug Fixes

* Busy wait to avoid race condition ([9d6dd1b](https://github.com/MaxG87/ButterBackup/commit/9d6dd1ba64775a6ecb785b99b8ab9f7eca6aefe4))
* Refuse to do backup on opened device ([34caba3](https://github.com/MaxG87/ButterBackup/commit/34caba3b197483992ecb13045d95800f6f3b89c9))
* Refuse to open already opened device ([7b9acb8](https://github.com/MaxG87/ButterBackup/commit/7b9acb8c3f4acfa56622a7b07efb57c364b24225))


### Dependencies

* Bump typer (v0.16.0) and pydantic (v2.11.7) ([aa5ebd3](https://github.com/MaxG87/ButterBackup/commit/aa5ebd38c75d8342649783b402db523de1ab16ec))
* **dev:** Bump all development dependencies ([0926918](https://github.com/MaxG87/ButterBackup/commit/0926918bf453221f1e32fb65f6ce965aedc0fd5d))
* **dev:** bump the development-dependencies group with 4 updates ([#79](https://github.com/MaxG87/ButterBackup/issues/79)) ([3e83246](https://github.com/MaxG87/ButterBackup/commit/3e832460e8dfe1fa5321f3fb36316c37674858c5))
* **dev:** bump the development-dependencies group with 7 updates ([#78](https://github.com/MaxG87/ButterBackup/issues/78)) ([d84b2fe](https://github.com/MaxG87/ButterBackup/commit/d84b2fe8f6c8830b777da75f9a0592271ab1c730))
* **dev:** Drop pynvim ([bde08cc](https://github.com/MaxG87/ButterBackup/commit/bde08ccc046167a3e6b6d3656a19457d56d43a46))


### Documentation

* Add project URLs ([e039cf7](https://github.com/MaxG87/ButterBackup/commit/e039cf7d99148086ebb691d2da988b7f67901778))
* Replace poetry with uv in README ([d9d17f8](https://github.com/MaxG87/ButterBackup/commit/d9d17f819702fdd6f8222888713d560c9985aba2))

## [3.5.1](https://github.com/MaxG87/ButterBackup/compare/v3.5.0...v3.5.1) (2025-02-17)


### Bug Fixes

* **ci:** Use publish pipeline based on uv and PyPI Github Action ([54f7639](https://github.com/MaxG87/ButterBackup/commit/54f763956426ec1ad36ee8c17326e12a48d84b3a))

## [3.5.0](https://github.com/MaxG87/ButterBackup/compare/v3.4.0...v3.5.0) (2025-02-17)


### Bug Fixes

* **ci:** Use correct automatic variable in pseudo phony target ([3d414e5](https://github.com/MaxG87/ButterBackup/commit/3d414e51c4db4e553bdc538ec3cd2d876c0e6751))
* **tests:** Use archlinux image that exists ([8cf8631](https://github.com/MaxG87/ButterBackup/commit/8cf86316e6aec79428df590c0f90c6b882d4eba2))
* The local dockerised test suite works again.


### Dependencies

* **Added support for Python 3.13!***
* **Removed upper limit of Python**, which should allow usage on upcomming Python versions. ([0710dee](https://github.com/MaxG87/ButterBackup/commit/0710deeba6bca605347ac4f9699ef21490768de9))
* Bump shell-interface and storage-device-managers to v1.0.0 ([ec23d13](https://github.com/MaxG87/ButterBackup/commit/ec23d131ba57927a70d2e7e28518e62c7cb7d027))
* **dev:** Bump pynvim to v0.5.2 ([af799dd](https://github.com/MaxG87/ButterBackup/commit/af799dd11e6d8d85e70d25fcb44d674725e1f040))
* **dev:** bump ruff in the development-dependencies group ([#76](https://github.com/MaxG87/ButterBackup/issues/76)) ([854abe7](https://github.com/MaxG87/ButterBackup/commit/854abe705cda4d51f6e311107f74fe91800fd658))
* Drop support for Python 3.8 ([ec53681](https://github.com/MaxG87/ButterBackup/commit/ec53681fcae1be070d9a6a7df7bc0134e0bffac6))
* Remove upper bound on all project dependencies ([de2e5ab](https://github.com/MaxG87/ButterBackup/commit/de2e5ab35a48d686d9f195383348c96cf3f640f5))
* Wechsle zu pydantic 2.0 ([b2d2aab](https://github.com/MaxG87/ButterBackup/commit/b2d2aab5ada00e08c4bfa1ac35c8737c1262cad6))


### Miscellaneous Chores

* release 3.5.0 ([9296019](https://github.com/MaxG87/ButterBackup/commit/92960193a649f4f4342c428e7a28ec1425c60f75))


### Other

* Switched to `uv` for project management.
* Include README in distribution, improving the representation on PyPI.


## [3.4.0](https://github.com/MaxG87/ButterBackup/compare/v3.3.2...v3.4.0) (2023-12-22)


### Features

* Erweitere Unterstützung auf Python 3.12 ([26b59c5](https://github.com/MaxG87/ButterBackup/commit/26b59c5156e218f9c8fc492ad402813747b1c2fa))


### Dependencies

* Hebe Version zweier Abhängigkeiten ([ef58a1e](https://github.com/MaxG87/ButterBackup/commit/ef58a1ee849694c1fef1f608046589b0453a39ff))


### Documentation

* Behebe kleinen Fehler in CHANGELOG ([31b0f7f](https://github.com/MaxG87/ButterBackup/commit/31b0f7f65fa990412706154bbf69908757c3dd80))

## [3.3.2](https://github.com/MaxG87/ButterBackup/compare/v3.3.1...v3.3.2) (2023-12-15)


### Features

* Ermögliche schöne CLI-Darstellung durch Rich mit `butter-backup[all]`


### Bug Fixes

* Reduziere Pythonunterstützung zu 3.12 ([6e6db2e](https://github.com/MaxG87/ButterBackup/commit/6e6db2e07062e2ffe1f873366923051f7b93e5e8))


### Dependencies

* Füge pytest-xdist als dev-dependency hinzu ([b335a8f](https://github.com/MaxG87/ButterBackup/commit/b335a8f75cbcb06a52304f9ba2d33f29f6242a4f))
* Relock aller Abhängigkeiten ([9c51437](https://github.com/MaxG87/ButterBackup/commit/9c514375dbb8f242cec07f0329ae38fa0542ccde))


### Documentation

* Überarbeite Abschnitt zu Testsuite in README ([c7cf981](https://github.com/MaxG87/ButterBackup/commit/c7cf981ddce76154c0d3ed43fbce8a811580aae6))
