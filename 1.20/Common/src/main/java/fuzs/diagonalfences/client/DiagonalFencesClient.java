package fuzs.diagonalfences.client;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.client.core.ClientAbstractions;
import fuzs.diagonalfences.client.handler.DiagonalModelHandler;
import fuzs.diagonalfences.data.client.DynamicModelProvider;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.event.v1.ModelEvents;
import fuzs.puzzleslib.api.core.v1.context.PackRepositorySourcesContext;
import fuzs.puzzleslib.api.event.v1.LoadCompleteCallback;
import fuzs.puzzleslib.api.resources.v1.DynamicPackResources;
import fuzs.puzzleslib.api.resources.v1.PackResourcesHelper;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.world.level.block.Block;

import java.util.Map;

public class DiagonalFencesClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerHandlers();
    }

    private static void registerHandlers() {
        ModelEvents.MODIFY_UNBAKED_MODEL.register(DiagonalModelHandler::onModifyUnbakedModel);
        LoadCompleteCallback.EVENT.register(() -> {
            // run a custom implementation here, the appropriate method in client mod constructor runs together with other mods, so we might miss some entries
            for (DiagonalBlockType type : DiagonalBlockType.values()) {
                for (Map.Entry<Block, Block> entry : type.getConversions().entrySet()) {
                    RenderType renderType = fuzs.puzzleslib.api.client.core.v1.ClientAbstractions.INSTANCE.getRenderType(entry.getKey());
                    ClientAbstractions.INSTANCE.registerRenderType(entry.getValue(), renderType);
                }
            }
        });
    }

    @Override
    public void onAddResourcePackFinders(PackRepositorySourcesContext context) {
        context.addRepositorySource(PackResourcesHelper.buildClientPack(DiagonalFences.id("default_block_models"), DynamicPackResources.create(DynamicModelProvider::new), true));
    }
}
