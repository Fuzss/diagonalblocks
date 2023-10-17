package fuzs.diagonalfences.client;

import fuzs.diagonalfences.client.handler.FenceModelHandler;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;

public class DiagonalFencesClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerHandlers();
    }

    private static void registerHandlers() {
        ModelEventsV2.MODIFY_UNBAKED_MODEL.register(FenceModelHandler::onModifyUnbakedModel);
    }
}
