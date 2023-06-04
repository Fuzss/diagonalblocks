package fuzs.diagonalfences;

import fuzs.diagonalfences.data.ModBlockTagsProvider;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;
import net.minecraftforge.forge.event.lifecycle.GatherDataEvent;

@Mod(DiagonalFences.MOD_ID)
@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD)
public class DiagonalFencesForge {

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        ModConstructor.construct(DiagonalFences.MOD_ID, DiagonalFences::new);
    }

    @SubscribeEvent
    public static void onGatherData(final GatherDataEvent evt) {
        final DataGenerator dataGenerator = evt.getGenerator();
        final ExistingFileHelper fileHelper = evt.getExistingFileHelper();
        dataGenerator.addProvider(new ModBlockTagsProvider(dataGenerator, DiagonalFences.MOD_ID, fileHelper));
    }
}
