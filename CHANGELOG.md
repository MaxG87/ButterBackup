# Changelog

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
