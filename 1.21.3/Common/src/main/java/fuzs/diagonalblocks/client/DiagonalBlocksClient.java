package fuzs.diagonalblocks.client;

import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.handler.DiagonalModelHandler;
import fuzs.diagonalblocks.client.resources.translator.WallMultiPartTranslator;
import fuzs.diagonalblocks.client.resources.translator.WindowMultiPartTranslator;
import fuzs.puzzleslib.api.client.core.v1.ClientAbstractions;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.event.v1.ModelEvents;
import fuzs.puzzleslib.api.event.v1.LoadCompleteCallback;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.world.level.block.Block;

import java.util.Map;

public class DiagonalBlocksClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerEventHandlers();
        // this cannot happen later during client setup,
        // as model loading will already have begun by then in the background
        MultiPartTranslator.register(DiagonalBlockTypes.WINDOW, new WindowMultiPartTranslator());
        MultiPartTranslator.register(DiagonalBlockTypes.WALL, new WallMultiPartTranslator());
    }

    private static void registerEventHandlers() {
        ModelEvents.COMPLETE_MODEL_LOADING.register(DiagonalModelHandler::onCompleteModelLoading);
        LoadCompleteCallback.EVENT.register(() -> {
            // run a custom implementation here, the appropriate method in client mod constructor runs together with other mods, so we might miss some entries
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                for (Map.Entry<Block, Block> entry : type.getBlockConversions().entrySet()) {
                    RenderType renderType = ClientAbstractions.INSTANCE.getRenderType(entry.getKey());
                    ClientAbstractions.INSTANCE.registerRenderType(entry.getValue(), renderType);
                }
            }
        });
    }
}
