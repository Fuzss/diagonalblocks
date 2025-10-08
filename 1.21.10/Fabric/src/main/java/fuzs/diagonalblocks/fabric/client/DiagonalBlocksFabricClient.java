package fuzs.diagonalblocks.fabric.client;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.client.DiagonalBlocksClient;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import net.fabricmc.api.ClientModInitializer;

public class DiagonalBlocksFabricClient implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        ClientModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocksClient::new);
    }
}
