package fuzs.diagonalfences.client;

import fuzs.diagonalfences.DiagonalFences;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ModelEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;

@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
public class DiagonalFencesForgeClient {

    @SubscribeEvent
    public static void onBakingCompleted(final ModelEvent.BakingCompleted evt) {
        DiagonalFencesClient.onBakingCompleted(evt.getModelManager(), evt.getModels(), evt.getModelBakery());
    }
}
