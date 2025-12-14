package fuzs.diagonalblocks.fabric.impl.client;

import fuzs.diagonalblocks.impl.DiagonalBlocks;
import fuzs.diagonalblocks.impl.client.DiagonalBlocksClient;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import net.fabricmc.api.ClientModInitializer;

public class DiagonalBlocksFabricClient implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        ClientModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocksClient::new);
    }
}
