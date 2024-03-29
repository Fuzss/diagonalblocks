# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog].

## [v8.0.5-1.20.1] - 2024-03-29
### Changed
- A few minor improvements towards better mod compatibility:
  - Prevent invalid model warnings for built-in blacklisted blocks
  - Disable Better End / Better Nether blocks from generating diagonal variants, instead of only blacklisting those blocks in the corresponding tags
  - Blacklist blocks from the Domum Ornamentum mod by default

## [v8.0.4-1.20.1] - 2023-12-08
### Fixed
- Fences from [Immersive Engineering](https://www.curseforge.com/minecraft/mc-mods/immersive-engineering) are now blacklisted by default to prevent issues with multi-blocks not forming

## [v8.0.3-1.20.1] - 2023-11-13
### Changed
- Use base block model as fallback if model transformation fails instead of defaulting to stone block model

## [v8.0.2-1.20.1] - 2023-10-29
### Changed
- Updated to Puzzles Lib v8.1.5

## [v8.0.1-1.20.1] - 2023-10-28
### Changed
- Do not target blocks that have more block state properties than expected, fixes certain replacement blocks for block subtypes from failing to construct due to invalid block properties

## [v8.0.0-1.20.1] - 2023-10-26
- Initial release

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
