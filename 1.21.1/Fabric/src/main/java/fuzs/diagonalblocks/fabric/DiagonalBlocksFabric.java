package fuzs.diagonalblocks.fabric;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import net.fabricmc.api.ModInitializer;

public class DiagonalBlocksFabric implements ModInitializer {

    @Override
    public void onInitialize() {
        ModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocks::new);
    }
}
