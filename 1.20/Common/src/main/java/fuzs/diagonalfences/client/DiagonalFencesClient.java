package fuzs.diagonalfences.client;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.data.client.DynamicModelProvider;
import fuzs.diagonalfences.client.handler.FenceModelHandler;
import fuzs.diagonalfences.client.handler.WallModelHandler;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.event.v1.ModelEvents;
import fuzs.puzzleslib.api.core.v1.context.PackRepositorySourcesContext;
import fuzs.puzzleslib.api.resources.v1.DynamicPackResources;
import fuzs.puzzleslib.api.resources.v1.PackResourcesHelper;

public class DiagonalFencesClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerHandlers();
    }

    private static void registerHandlers() {
        ModelEvents.MODIFY_UNBAKED_MODEL.register(FenceModelHandler::onModifyUnbakedModel);
        ModelEvents.MODIFY_UNBAKED_MODEL.register(WallModelHandler::onModifyUnbakedModel);
    }

    @Override
    public void onAddResourcePackFinders(PackRepositorySourcesContext context) {
        context.addRepositorySource(PackResourcesHelper.buildClientPack(DiagonalFences.id("dynamic_walls"), DynamicPackResources.create(DynamicModelProvider::new), true));
    }
}
