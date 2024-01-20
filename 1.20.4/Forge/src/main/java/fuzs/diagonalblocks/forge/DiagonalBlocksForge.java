package fuzs.diagonalblocks.forge;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.forge.integration.cfm.MrCrayfishFurnitureMod;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;

@Mod(DiagonalBlocks.MOD_ID)
@Mod.EventBusSubscriber(modid = DiagonalBlocks.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD)
public class DiagonalBlocksForge {

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        ModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocks::new);
        registerIntegration();
    }

    private static void registerIntegration() {
        MrCrayfishFurnitureMod.init();
    }
}
