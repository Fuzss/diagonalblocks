package fuzs.diagonalblocks;

import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.handler.DiagonalBlockHandler;
import fuzs.diagonalblocks.init.ModRegistry;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.api.core.v1.ModLoaderEnvironment;
import fuzs.puzzleslib.api.core.v1.utility.ResourceLocationHelper;
import fuzs.puzzleslib.api.event.v1.core.EventPhase;
import fuzs.puzzleslib.api.event.v1.entity.player.PlayerInteractEvents;
import fuzs.puzzleslib.api.event.v1.server.TagsUpdatedCallback;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DiagonalBlocks implements ModConstructor {
    public static final String MOD_ID = "diagonalblocks";
    public static final String MOD_NAME = "Diagonal Blocks";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalBlocks.MOD_NAME);

    @Override
    public void onConstructMod() {
        if (ModLoaderEnvironment.INSTANCE.isDevelopmentEnvironment(MOD_ID)) {
            DiagonalBlockType.register(DiagonalBlockTypes.FENCE);
            DiagonalBlockType.register(DiagonalBlockTypes.WINDOW);
            DiagonalBlockType.register(DiagonalBlockTypes.WALL);
        }
        ModRegistry.bootstrap();
        registerEventHandlers();
    }

    private static void registerEventHandlers() {
        TagsUpdatedCallback.EVENT.register(EventPhase.FIRST, DiagonalBlockHandler::onTagsUpdated);
        PlayerInteractEvents.USE_BLOCK.register(DiagonalBlockHandler::onUseBlock);
    }

    public static ResourceLocation id(String path) {
        return ResourceLocationHelper.fromNamespaceAndPath(MOD_ID, path);
    }
}
