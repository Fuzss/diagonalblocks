# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v21.1.1-1.21.1] - 2024-11-18
### Changed
- Use `RegisterClientExtensionsEvent` for registering custom `IClientBlockExtensions` on NeoForge
- Register specialized `MultiPartTranslator` instances during mod construction instead of setup phase to avoid issues with some optimization mods beginning to load models much earlier than expected

## [v21.1.0-1.21.1] - 2024-09-17
- Port to Minecraft 1.21.1
