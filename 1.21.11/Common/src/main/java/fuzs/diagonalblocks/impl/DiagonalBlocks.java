package fuzs.diagonalblocks.impl;

import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockTypes;
import fuzs.diagonalblocks.impl.handler.DiagonalBlockHandler;
import fuzs.diagonalblocks.impl.init.ModRegistry;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.api.core.v1.ModLoaderEnvironment;
import net.minecraft.resources.Identifier;
import fuzs.puzzleslib.api.event.v1.core.EventPhase;
import fuzs.puzzleslib.api.event.v1.entity.player.PlayerInteractEvents;
import fuzs.puzzleslib.api.event.v1.server.TagsUpdatedCallback;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DiagonalBlocks implements ModConstructor {
    public static final String MOD_ID = "diagonalblocks";
    public static final String MOD_NAME = "Diagonal Blocks";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalBlocks.MOD_NAME);

    @Override
    public void onConstructMod() {
        ModRegistry.bootstrap();
        if (ModLoaderEnvironment.INSTANCE.isDevelopmentEnvironment(MOD_ID)) {
            DiagonalBlockType.register(DiagonalBlockTypes.FENCE);
            DiagonalBlockType.register(DiagonalBlockTypes.WINDOW);
            DiagonalBlockType.register(DiagonalBlockTypes.WALL);
        }
    }

    @Override
    public void onCommonSetup() {
        registerEventHandlers();
    }

    private static void registerEventHandlers() {
        TagsUpdatedCallback.EVENT.register(EventPhase.FIRST, DiagonalBlockHandler::onTagsUpdated);
        PlayerInteractEvents.USE_BLOCK.register(DiagonalBlockHandler::onUseBlock);
    }

    public static Identifier id(String path) {
        return Identifier.fromNamespaceAndPath(MOD_ID, path);
    }
}
