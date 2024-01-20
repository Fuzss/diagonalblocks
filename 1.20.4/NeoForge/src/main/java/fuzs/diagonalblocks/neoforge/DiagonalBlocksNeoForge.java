package fuzs.diagonalblocks.neoforge;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.data.ModBlockTagsProvider;
import fuzs.diagonalblocks.neoforge.integration.cfm.MrCrayfishFurnitureMod;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.neoforge.api.data.v2.core.DataProviderHelper;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLConstructModEvent;

@Mod(DiagonalBlocks.MOD_ID)
@Mod.EventBusSubscriber(modid = DiagonalBlocks.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD)
public class DiagonalBlocksNeoForge {

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        ModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocks::new);
        DataProviderHelper.registerDataProviders(DiagonalBlocks.MOD_ID, ModBlockTagsProvider::new);
        registerIntegration();
    }

    private static void registerIntegration() {
        MrCrayfishFurnitureMod.init();
    }
}
