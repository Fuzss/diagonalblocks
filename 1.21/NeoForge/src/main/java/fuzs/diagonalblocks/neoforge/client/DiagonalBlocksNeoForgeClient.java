package fuzs.diagonalblocks.neoforge.client;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.client.DiagonalBlocksClient;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.fml.common.Mod;

@Mod(value = DiagonalBlocks.MOD_ID, dist = Dist.CLIENT)
public class DiagonalBlocksNeoForgeClient {

    public DiagonalBlocksNeoForgeClient() {
        ClientModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocksClient::new);
    }
}
