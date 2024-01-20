package fuzs.diagonalblocks;

import fuzs.diagonalblocks.data.ModBlockTagsProvider;
import fuzs.diagonalblocks.integration.cfm.MrCrayfishFurnitureMod;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.api.data.v2.core.DataProviderHelper;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;

@Mod(DiagonalBlocks.MOD_ID)
@Mod.EventBusSubscriber(modid = DiagonalBlocks.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD)
public class DiagonalBlocksForge {

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
