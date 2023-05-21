# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog].

## [v4.2.5-1.19.2] - 2023-05-21
### Fixed
- Fixed small animals (such as chicken and rabbits) trying to path-find through diagonal fence connections

## [v4.2.4-1.19.2] - 2023-05-12
### Changed
- Overhauled internal implementation for making model parts diagonal, possibly allowing for future support of new kinds of blocks such as diagonal glass panes
### Fixed
- Fixed a long-standing issue where Diagonal Fences would duplicate block models for every single block state, whereas vanilla would only use a single model with different selectors applied; leading to extremely high memory usage, making the game easily run out of heap space when paired with other mods that add a lot of fences
### Removed
- Removed the client config, due to internal changes all available options are no longer necessary
- Removed dedicated support for the [Lambda Better Grass](https://www.curseforge.com/minecraft/mc-mods/lambdabettergrass) mod, the mod now just works out of the box with the new implementation

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