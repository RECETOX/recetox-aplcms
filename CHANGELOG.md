# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [dev] - now
### Added
- implemented maximum elution time filter [#243](https://github.com/RECETOX/recetox-aplcms/pull/243)

## [0.14.0] - 2025-07-04
### Added
- implemented reading .raw files using rawRR [#237](https://github.com/RECETOX/recetox-aplcms/pull/237)

### Changed
- changed peak ids from float to integer [#236](https://github.com/RECETOX/recetox-aplcms/pull/236)

## [0.13.4] - 2024-10-30
### Changed
- re-enabled parallelism in alignment step [#230](https://github.com/RECETOX/recetox-aplcms/pull/230)

## [0.13.3] - 2024-09-16
### Changed
- fixed not working eic splitting code [#226](https://github.com/RECETOX/recetox-aplcms/pull/226)

## [0.13.2] - 2024-07-30
### Changed
- Moved import order in the hope it will fix the import bugs by @hechth in [#224](https://github.com/RECETOX/recetox-aplcms/pull/224)


## [0.13.0] - 2024-07-29
### Added
- added tests for mzdata and mzxml formats [#216](https://github.com/RECETOX/recetox-aplcms/pull/216)

### Changed
- feature.alignment function refactored by @KristinaGomoryova in [#221](https://github.com/RECETOX/recetox-aplcms/pull/221)
- remove_noise function updated by @KristinaGomoryova in [#219](https://github.com/RECETOX/recetox-aplcms/pull/219)
- Updated correct time to use precomputed clusters by @zargham-ahmad in [#220](https://github.com/RECETOX/recetox-aplcms/pull/220)
- Fix tests by @hechth in [#222](https://github.com/RECETOX/recetox-aplcms/pull/222)

### Removed
- removed outdated files by @hechth in [#223](https://github.com/RECETOX/recetox-aplcms/pull/223)

## [0.12.0] - 2023-07-10
### Changed
- sample names in aligned tables no longer use _intensity and _rt suffixes [#212](https://github.com/RECETOX/recetox-aplcms/pull/212)
### Removed
- corner case in peak picking when an EIC was between 2 and 10 features [#212](https://github.com/RECETOX/recetox-aplcms/pull/212)

## [0.11.0] - 2023-05-09
### Added
 - implemented new clustering algorithm and included parallelism in unsupervised and hybrid [#201](https://github.com/RECETOX/recetox-aplcms/pull/201)
### Changed
- refactored adaptive.bin and combine.seq.3 [#196](https://github.com/RECETOX/recetox-aplcms/pull/196)
- refactored find.match [#193](https://github.com/RECETOX/recetox-aplcms/pull/193)
- Simplified Evaluation Conditions For Test Cases [#197](https://github.com/RECETOX/recetox-aplcms/pull/197)
- Renamed `proc.cdf` to `remove_noise` [#190](https://github.com/RECETOX/recetox-aplcms/pull/190)
- Fixed dimension bug `adaptive.bin` [#200](https://github.com/RECETOX/recetox-aplcms/pull/200)
### Removed

## [0.10.3] - 2023-03-27
### Changed
- introduced parallelism in feature.align [#182](https://github.com/RECETOX/recetox-aplcms/pull/182)

## [0.10.2] - 2023-03-10
### Added
### Changed
- fixed bug in recover.weaker related to sample name matching [#180](https://github.com/RECETOX/recetox-aplcms/pull/180)
### Removed

## [0.10.1] - 2023-02-02

### Added
- refactored `rm.ridge.R` [#171](https://github.com/RECETOX/recetox-aplcms/pull/171)
- refactored and documented `prof.to.features.R` [#170](https://github.com/RECETOX/recetox-aplcms/pull/170)
- added full testdata case for `unsupervised.R` and `hybrid.R` [#177](https://github.com/RECETOX/recetox-aplcms/pull/177)
- added function to sort data in `compute_clusters.R` to return sorted data [#177](https://github.com/RECETOX/recetox-aplcms/pull/177)

### Changed
- updated remote files with the full data get links [#177](https://github.com/RECETOX/recetox-aplcms/pull/177)
- fixed parameter value of recover.weaker in `unsupervised.R` and `hybrid.R` [#177](https://github.com/RECETOX/recetox-aplcms/pull/177)
- reinstated returning tolerances from clusters and passed them in `unsupervised.R` and `hybrid.R` [#178](https://github.com/RECETOX/recetox-aplcms/pull/178)

### Removed
- removed NA check in `concatenate_feature_tables` [#177](https://github.com/RECETOX/recetox-aplcms/pull/177)

## [0.10.0] - 2022-12-07

### Added
- added documentation from Rd files to code [#78](https://github.com/RECETOX/recetox-aplcms/pull/78)
- added tests with realistic testdata for `extract_features.R` [#42](https://github.com/RECETOX/recetox-aplcms/pull/42), [#54](https://github.com/RECETOX/recetox-aplcms/pull/54)
- added tests for `feature.align.R` ([#40](https://github.com/RECETOX/recetox-aplcms/pull/40)), and `adjust.time.R` ([#39](https://github.com/RECETOX/recetox-aplcms/pull/40))
- added CI to repository's GitHub Actions [#45](https://github.com/RECETOX/recetox-aplcms/pull/45),[#49](https://github.com/RECETOX/recetox-aplcms/pull/49)
- added additional test cases for hybrid [#133](https://github.com/RECETOX/recetox-aplcms/pull/133)
- added tests and testdata for run_filter.R [#156](https://github.com/RECETOX/recetox-aplcms/pull/156)
- `merge_features_and_known_table` wrapper for augmentation of aligned features and known table [#154](https://github.com/RECETOX/recetox-aplcms/pull/154)

### Changed
- refactored `feature.align.R` [#63](https://github.com/RECETOX/recetox-aplcms/pull/63)[#88](https://github.com/RECETOX/recetox-aplcms/pull/88)[#102](https://github.com/RECETOX/recetox-aplcms/pull/102)
- refactored `adjust.time.R` [#64](https://github.com/RECETOX/recetox-aplcms/pull/64)[#102](https://github.com/RECETOX/recetox-aplcms/pull/102)
- refactored `find.tol.time.R` [#91](https://github.com/RECETOX/recetox-aplcms/pull/91)
- refactored `find.turn.point.R` [#91](https://github.com/RECETOX/recetox-aplcms/pull/91)
- refactored `proc.cdf.R` and `adaptive.bin.R` [#137](https://github.com/RECETOX/recetox-aplcms/pull/137)
- refactored `cont.index.R` and renamed as `run_filter.R` [#156](https://github.com/RECETOX/recetox-aplcms/pull/156)
- use proper sample IDs inside feature tables [#153](https://github.com/RECETOX/recetox-aplcms/pull/153)
- exported functions in NAMESPACE [#154](https://github.com/RECETOX/recetox-aplcms/pull/154)
- docstrings and documentation files for refactored functions [#160](https://github.com/RECETOX/recetox-aplcms/pull/160)
- refactored parameter names to keep them more harmonized [#167](https://github.com/RECETOX/recetox-aplcms/pull/167)
- moved some utility functions to a more suitable locations [#164](https://github.com/RECETOX/recetox-aplcms/pull/164)

### Removed
- `extract_features` and `feature.align` [#154](https://github.com/RECETOX/recetox-aplcms/pull/154)
- improper usage of `@examples` [#160](https://github.com/RECETOX/recetox-aplcms/pull/160)
- several obsolete utility functions [#164](https://github.com/RECETOX/recetox-aplcms/pull/164)
- several outdated `.Rd` files [#168](https://github.com/RECETOX/recetox-aplcms/pull/168)
- default argument values from low-level functions [#168](https://github.com/RECETOX/recetox-aplcms/pull/168)

## [0.9.4] - 2022-05-10

### Added
- added complete tests for unsupervised and hybrid versions [#19](https://github.com/RECETOX/recetox-aplcms/pull/19)
- added conda environments [#20](https://github.com/RECETOX/recetox-aplcms/pull/20)
- added Docker development environment [#31](https://github.com/RECETOX/recetox-aplcms/pull/31)
- added VSCode [devcontainer](https://code.visualstudio.com/docs/remote/containers) configuration [#31](https://github.com/RECETOX/recetox-aplcms/pull/31)

### Changed
- fixed repository to comply with `devtools::check()` [#27](https://github.com/RECETOX/recetox-aplcms/pull/27)
- refactored `two.step.hybrid.R` [#34](https://github.com/RECETOX/recetox-aplcms/pull/34)
