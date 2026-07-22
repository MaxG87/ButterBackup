# Changelog

## [3.5.0](https://github.com/MaxG87/ButterBackup/compare/v2.0.0...v3.5.0) (2026-07-22)


### ⚠ BREAKING CHANGES

* The libraries storage-device-managers and shell-interface now don't log any output. Code that configured them accordingly, or depended on that, will break.

### Dependencies

* Bump all prod and dev dependencies ([761d3e5](https://github.com/MaxG87/ButterBackup/commit/761d3e5a73ae26eabc04d5b5384726dfa4d72177))
* **dev:** bump hypothesis in the development-dependencies group ([#128](https://github.com/MaxG87/ButterBackup/issues/128)) ([799133a](https://github.com/MaxG87/ButterBackup/commit/799133ac77ab54e4cd215acc6ce916624f6158ab))


### Miscellaneous Chores

* release 3.5.0 ([9296019](https://github.com/MaxG87/ButterBackup/commit/92960193a649f4f4342c428e7a28ec1425c60f75))
* release v0.15.0 ([bd5bfcc](https://github.com/MaxG87/ButterBackup/commit/bd5bfcc8238268cc7c8d3c69af47f37fed175020))


### Code Refactoring

* Drop loguru from both libraries ([bdacc96](https://github.com/MaxG87/ButterBackup/commit/bdacc960d1a94b121f8401b1ed57f17ee5a3c241))

## [2.0.0](https://github.com/MaxG87/shell-interface/compare/v1.0.2...v2.0.0) (2026-04-26)


### ⚠ BREAKING CHANGES

* Raise dedicated exception on password command failure

### Features

* Raise dedicated exception on password command failure ([d986436](https://github.com/MaxG87/shell-interface/commit/d98643609b70eb2670eba1aae80e1bb1978f8723))


### Dependencies

* **dev:** Bump all development dependencies ([9023a79](https://github.com/MaxG87/shell-interface/commit/9023a79e6c5d0b5d538725559eaa8353383a0883))

## [1.0.2](https://github.com/MaxG87/shell-interface/compare/v1.0.1...v1.0.2) (2026-01-10)


### Dependencies

* **dev:** Bump all dev-dependencies to latest version ([12396c6](https://github.com/MaxG87/shell-interface/commit/12396c6da4b2c76b406b6131897952d1d963387d))
* **dev:** Drop pynvim and jedi ([f6846ad](https://github.com/MaxG87/shell-interface/commit/f6846ad0845a17e41be3d8559dd3f255735b1404))

## [1.0.1](https://github.com/MaxG87/shell-interface/compare/v1.0.0...v1.0.1) (2025-07-02)


### Dependencies

* **dev:** Bump all development dependencies ([54ece2b](https://github.com/MaxG87/shell-interface/commit/54ece2b2b0beb26b0f4c844723fab41e11a65d4e))

## [1.0.0](https://github.com/MaxG87/shell-interface/compare/v0.15.2...v1.0.0) (2025-02-15)


### Bug Fixes

* **ci:** Run publish jobs sequentially ([12d49f2](https://github.com/MaxG87/shell-interface/commit/12d49f2e0401809ca050be6012431cb4d9d3f2ec))


### Documentation

* Add generated README ([5cabbc6](https://github.com/MaxG87/shell-interface/commit/5cabbc63e6b0ae21c7359656a53555258cd84cd2))
* Add some metadata URLs ([68cae4a](https://github.com/MaxG87/shell-interface/commit/68cae4a8869454db4dd6e8a21348c5cc8d4c76c5))

## [0.15.2](https://github.com/MaxG87/shell-interface/compare/v0.15.1...v0.15.2) (2025-02-13)


### Bug Fixes

* **ci:** Use valid job name ([04fa985](https://github.com/MaxG87/shell-interface/commit/04fa9855666c7ff2ece3adcadc3386329d6fcd85))

## [0.15.1](https://github.com/MaxG87/shell-interface/compare/v0.15.0...v0.15.1) (2025-02-13)


### Continuous Integration

* Use PyPI publish Github Action ([b13ae36](https://github.com/MaxG87/shell-interface/commit/b13ae3607062e1fc651cf2461de5bd03ab48fdec))

## [0.15.0](https://github.com/MaxG87/shell-interface/compare/v0.14.0...v0.15.0) (2025-02-04)

### Dependencies

* All lower bounds on dependencies have been raised.
* All upper bounds on dependencies have been **removed**!
* The upper bound on Python itself has been **removed**!

### Miscellaneous Chores

* release v0.15.0 ([bd5bfcc](https://github.com/MaxG87/shell-interface/commit/bd5bfcc8238268cc7c8d3c69af47f37fed175020))

## [0.14.0](https://github.com/MaxG87/shell-interface/compare/v0.13.0...v0.14.0) (2024-11-25)


### Miscellaneous Chores

* release 0.14.0 ([ccff8ce](https://github.com/MaxG87/shell-interface/commit/ccff8cedd9967678e41bfec8574a1007f4fd4723))

## [0.13.0](https://github.com/MaxG87/shell-interface/compare/v0.12.0...v0.13.0) (2023-12-22)


### Features

* Add Python 3.12 as supported version ([0cab641](https://github.com/MaxG87/shell-interface/commit/0cab641a57d372c97152b8a53f24e5a14e26708a))


### Documentation

* Add CHANGELOG.md ([66db93c](https://github.com/MaxG87/shell-interface/commit/66db93ccea5c3cbee2e8a5db0c59f37e1c092651))

## v0.12.0

* improve metadata for better PyPI appereance

## v0.11.1

* fix minior issues in Workflow files

## v0.11.0

* forking off of ButterBackup completed
