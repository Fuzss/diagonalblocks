package fuzs.diagonalfences.client;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.puzzleslib.client.core.ClientCoreServices;
import net.fabricmc.api.ClientModInitializer;

public class DiagonalFencesFabricClient implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        ClientCoreServices.FACTORIES.clientModConstructor(DiagonalFences.MOD_ID).accept(new DiagonalFencesClient());
    }
}
