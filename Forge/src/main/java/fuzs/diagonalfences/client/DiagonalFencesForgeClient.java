package fuzs.diagonalfences.client;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.puzzleslib.client.core.ClientCoreServices;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;

@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
public class DiagonalFencesForgeClient {

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        ClientCoreServices.FACTORIES.clientModConstructor(DiagonalFences.MOD_ID).accept(new DiagonalFencesClient());
    }
}
