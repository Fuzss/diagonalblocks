# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog].

## [v3.2.1-1.18.2] - 2023-06-20
### Changed
- Diagonal Fences now supports slightly more complex block models from other mods / resource packs, so some fences that would gain diagonal connections but wouldn't render them will do so now
### Fixed
- Fixed unintentional breaking api change

## [v3.2.0-1.18.2] - 2023-06-04
- Backported to Minecraft 1.18.2

## [v3.1.1-1.18.2] - 2023-05-12
Backported v4.2.4 to fix a long-standing issue where Diagonal Fences would duplicate block models for every single block state, whereas vanilla would only use a single model with different selectors applied; leading to extremely high memory usage, making the game easily run out of heap space when paired with other mods that add a lot of fences

## [v3.1.0-1.18.2] - 2022-05-02
### Changed
- Replace virtual resource pack with post-bake transformations, once again all done by and thanks to [XFactHD]

## [v3.0.0-1.18.2] - 2022-04-25
- Ported to Minecraft 1.18.2 thanks to [XFactHD]

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
