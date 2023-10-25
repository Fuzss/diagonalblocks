package fuzs.diagonalfences;

import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.api.v2.DiagonalBlockTypes;
import fuzs.diagonalfences.handler.DiagonalBlockHandler;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.api.event.v1.core.EventPhase;
import fuzs.puzzleslib.api.event.v1.server.TagsUpdatedCallback;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DiagonalFences implements ModConstructor {
    public static final String MOD_ID = "diagonalfences";
    public static final String MOD_NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.MOD_NAME);

    @Override
    public void onConstructMod() {
        DiagonalBlockType.register(DiagonalBlockTypes.FENCE);
        DiagonalBlockType.register(DiagonalBlockTypes.WINDOW);
        DiagonalBlockType.register(DiagonalBlockTypes.WALL);
        registerHandlers();
    }

    private static void registerHandlers() {
        TagsUpdatedCallback.EVENT.register(EventPhase.FIRST, DiagonalBlockHandler::onTagsUpdated);
    }

    public static ResourceLocation id(String path) {
        return new ResourceLocation(MOD_ID, path);
    }
}
