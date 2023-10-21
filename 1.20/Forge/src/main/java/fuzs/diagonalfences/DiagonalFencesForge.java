package fuzs.diagonalfences;

import fuzs.diagonalfences.data.ModBlockTagsProvider;
import fuzs.diagonalfences.data.ModLanguageProvider;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.api.data.v2.core.DataProviderHelper;
import net.minecraftforge.data.event.GatherDataEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;

@Mod(DiagonalFences.MOD_ID)
@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD)
public class DiagonalFencesForge {

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        ModConstructor.construct(DiagonalFences.MOD_ID, DiagonalFences::new);
        DataProviderHelper.registerDataProviders(DiagonalFences.MOD_ID, ModLanguageProvider::new);
    }

    @SubscribeEvent
    public static void onGatherData(final GatherDataEvent evt) {
        evt.getGenerator().addProvider(evt.includeClient(), new ModBlockTagsProvider(evt, DiagonalFences.MOD_ID));
    }
}
