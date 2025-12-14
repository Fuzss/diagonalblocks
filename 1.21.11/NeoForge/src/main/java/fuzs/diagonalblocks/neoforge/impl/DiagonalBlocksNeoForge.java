package fuzs.diagonalblocks.neoforge.impl;

import fuzs.diagonalblocks.impl.DiagonalBlocks;
import fuzs.diagonalblocks.impl.data.ModBlockTagsProvider;
import fuzs.diagonalblocks.neoforge.integration.cfm.MrCrayfishFurnitureMod;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.neoforge.api.data.v2.core.DataProviderHelper;
import net.neoforged.fml.common.Mod;

@Mod(DiagonalBlocks.MOD_ID)
public class DiagonalBlocksNeoForge {

    public DiagonalBlocksNeoForge() {
        ModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocks::new);
        DataProviderHelper.registerDataProviders(DiagonalBlocks.MOD_ID, ModBlockTagsProvider::new);
        registerModIntegration();
    }

    private static void registerModIntegration() {
        MrCrayfishFurnitureMod.init();
    }
}
