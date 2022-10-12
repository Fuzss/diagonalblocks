# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog].

## [v4.2.3-1.19.2] - 2022-10-12
### Changed
- The time it took to construct diagonal fence shapes is now printed to the log to be better able to diagnose potential performance issues
### Fixed
- No longer crashes without any message when Puzzles Lib is missing, you should now properly be yelled at by the mod loader itself
- Fixed placing fences deleting water source blocks

## [v4.2.2-1.19.2] - 2022-10-12
### Added
- Added compatibility with Lambda Better Grass mod

## [v4.2.1-1.19.2] - 2022-08-29
### Changed
- Adding custom models for diagonal fences via resource packs is now supported

## [v4.2.0-1.19.2] - 2022-08-21
- Compiled for Minecraft 1.19.2

## [v4.1.0-1.19.1] - 2022-08-01
- Compiled for Minecraft 1.19.1
- Updated to Puzzles Lib v4.1.0
### Fixed
- Fixed too many particles appearing when a diagonal fence is broken, thanks to [XFactHD] for pointing me into the right direction!

## [v4.0.1-1.19] - 2022-07-15
### Fixed
- Fixed start-up crash on Forge due to methods in mixin classes not being properly obfuscated

## [v4.0.0-1.19] - 2022-07-14
- Ported to Minecraft 1.19 and Fabric for the first time
- Split into multi-loader project

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[XFactHD]: https://github.com/XFactHD