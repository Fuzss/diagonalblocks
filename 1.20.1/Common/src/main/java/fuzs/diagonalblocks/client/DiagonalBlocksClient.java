package fuzs.diagonalblocks.client;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.core.ClientAbstractions;
import fuzs.diagonalblocks.client.handler.DiagonalModelHandler;
import fuzs.diagonalblocks.client.resources.translator.WallMultiPartTranslator;
import fuzs.diagonalblocks.client.resources.translator.WindowMultiPartTranslator;
import fuzs.diagonalblocks.data.client.DynamicModelProvider;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.event.v1.ModelEvents;
import fuzs.puzzleslib.api.core.v1.context.PackRepositorySourcesContext;
import fuzs.puzzleslib.api.event.v1.LoadCompleteCallback;
import fuzs.puzzleslib.api.resources.v1.DynamicPackResources;
import fuzs.puzzleslib.api.resources.v1.PackResourcesHelper;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.world.level.block.Block;

import java.util.Map;

public class DiagonalBlocksClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerEventHandlers();
        MultiPartTranslator.register(DiagonalBlockTypes.WINDOW, new WindowMultiPartTranslator());
        MultiPartTranslator.register(DiagonalBlockTypes.WALL, new WallMultiPartTranslator());
    }

    private static void registerEventHandlers() {
        ModelEvents.MODIFY_UNBAKED_MODEL.register(DiagonalModelHandler::onModifyUnbakedModel);
        ModelEvents.AFTER_MODEL_LOADING.register(DiagonalModelHandler::onAfterModelLoading);
        LoadCompleteCallback.EVENT.register(() -> {
            // run a custom implementation here, the appropriate method in client mod constructor runs together with other mods, so we might miss some entries
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                for (Map.Entry<Block, Block> entry : type.getBlockConversions().entrySet()) {
                    RenderType renderType = fuzs.puzzleslib.api.client.core.v1.ClientAbstractions.INSTANCE.getRenderType(
                            entry.getKey());
                    ClientAbstractions.INSTANCE.registerRenderType(entry.getValue(), renderType);
                }
            }
        });
    }

    @Override
    public void onAddResourcePackFinders(PackRepositorySourcesContext context) {
        context.addRepositorySource(PackResourcesHelper.buildClientPack(DiagonalBlocks.id("default_block_models"),
                DynamicPackResources.create(DynamicModelProvider::new), true
        ));
    }
}
