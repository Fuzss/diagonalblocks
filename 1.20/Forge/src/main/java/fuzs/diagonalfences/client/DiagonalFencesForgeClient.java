package fuzs.diagonalfences.client;

import com.google.common.base.Stopwatch;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import fuzs.puzzleslib.api.event.v1.core.ForgeEventInvokerRegistry;
import net.minecraft.client.resources.model.*;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ModelEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;

import java.util.Map;
import java.util.stream.Collectors;

@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
public class DiagonalFencesForgeClient {

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        ClientModConstructor.construct(DiagonalFences.MOD_ID, DiagonalFencesClient::new);
        registerEventInvokers();
    }

    private static void registerEventInvokers() {
        ForgeEventInvokerRegistry.INSTANCE.register(ModelEventsV2.ModifyUnbakedModel.class, ModelEvent.ModifyBakingResult.class, (ModelEventsV2.ModifyUnbakedModel callback, ModelEvent.ModifyBakingResult evt) -> {
            Stopwatch stopwatch = Stopwatch.createStarted();
            Map<ResourceLocation, UnbakedModel> additionalModels = Maps.newHashMap();
            Multimap<ResourceLocation, Material> missingTextures = HashMultimap.create();
            Map<UnbakedModel, BakedModel> bakedModels = Maps.newIdentityHashMap();
            ModelBakery modelBakery = evt.getModelBakery();
            for (Map.Entry<ResourceLocation, BakedModel> entry : evt.getModels().entrySet()) {
                ResourceLocation resourceLocation = entry.getKey();
                UnbakedModel unbakedModel = modelBakery.getModel(resourceLocation);
                BakedModel bakedModel = bakedModels.get(unbakedModel);
                if (bakedModel == null) {
                    EventResultHolder<UnbakedModel> result = callback.onModifyUnbakedModel(resourceLocation, unbakedModel, modelBakery::getModel, additionalModels::put);
                    if (result.isInterrupt()) {
                        additionalModels.put(resourceLocation, result.getInterrupt().get());
                        bakedModel = new ForgeModelBakerImpl(resourceLocation, modelBakery::getModel, additionalModels::get, missingTextures::put).bake(resourceLocation, BlockModelRotation.X0_Y0);
                        bakedModels.put(unbakedModel, bakedModel);
                    }
                }
                if (bakedModel != null) {
                    entry.setValue(bakedModel);
                }
            }
            missingTextures.asMap().forEach((resourceLocation, materials) -> {
                DiagonalFences.LOGGER.warn("Missing textures in model {}:\n{}", resourceLocation, materials.stream().sorted(Material.COMPARATOR).map((material) -> {
                    return "    " + material.atlasLocation() + ":" + material.texture();
                }).collect(Collectors.joining("\n")));
            });
            DiagonalFences.LOGGER.info("Modifying unbaked models took {} millisecond(s)", stopwatch.stop().elapsed().toMillis());
        });
        ForgeEventInvokerRegistry.INSTANCE.register(ModelEventsV2.ModifyBakedModel.class, ModelEvent.ModifyBakingResult.class, (ModelEventsV2.ModifyBakedModel callback, ModelEvent.ModifyBakingResult evt) -> {
            Stopwatch stopwatch = Stopwatch.createStarted();
            Map<ResourceLocation, BakedModel> additionalModels = Maps.newLinkedHashMap();
            Multimap<ResourceLocation, Material> missingTextures = HashMultimap.create();
            for (Map.Entry<ResourceLocation, BakedModel> entry : evt.getModels().entrySet()) {
                EventResultHolder<BakedModel> result = callback.onModifyBakedModel(entry.getKey(), entry.getValue(), () -> {
                    return new ForgeModelBakerImpl(entry.getKey(), evt.getModelBakery()::getModel, missingTextures::put);
                }, evt.getModels()::get, additionalModels::put);
                result.getInterrupt().ifPresent(entry::setValue);
            }
            evt.getModels().putAll(additionalModels);
            missingTextures.asMap().forEach((resourceLocation, materials) -> {
                DiagonalFences.LOGGER.warn("Missing textures in model {}:\n{}", resourceLocation, materials.stream().sorted(Material.COMPARATOR).map((material) -> {
                    return "    " + material.atlasLocation() + ":" + material.texture();
                }).collect(Collectors.joining("\n")));
            });
            DiagonalFences.LOGGER.info("Modifying baked models took {} millisecond(s)", stopwatch.stop().elapsed().toMillis());
        });
    }
}
