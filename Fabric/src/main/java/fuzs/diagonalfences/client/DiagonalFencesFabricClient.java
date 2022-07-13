package fuzs.diagonalfences.client;

import fuzs.diagonalfences.api.client.event.ModelEvents;
import net.fabricmc.api.ClientModInitializer;

public class DiagonalFencesFabricClient implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        ModelEvents.BAKING_COMPLETED.register(DiagonalFencesClient::onBakingCompleted);
    }
}
