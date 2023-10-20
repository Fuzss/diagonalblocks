package fuzs.diagonalfences.client;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.client.handler.FenceModelHandler;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import net.minecraft.client.resources.model.ModelResourceLocation;

public class DiagonalFencesClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerHandlers();
    }

    private static void registerHandlers() {
        ModelEventsV2.MODIFY_UNBAKED_MODEL.register(FenceModelHandler::onModifyUnbakedModel);
        ModelEventsV2.MODIFY_BAKED_MODEL.register((modelLocation, bakedModel, modelBaker, modelGetter, modelAdder) -> {
            if (modelLocation.getNamespace().equals(DiagonalFences.MOD_ID)) {
                return EventResultHolder.interrupt(modelGetter.apply(new ModelResourceLocation("minecraft", "stone", "")));
            }
            return EventResultHolder.pass();
        });
    }
}
