package fuzs.diagonalfences.client;

import com.google.common.base.Stopwatch;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import fuzs.puzzleslib.api.event.v1.core.ForgeEventInvokerRegistry;
import fuzs.puzzleslib.impl.PuzzlesLib;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.Material;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ModelEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
public class DiagonalFencesForgeClient {

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        registerEventInvokers();
        ClientModConstructor.construct(DiagonalFences.MOD_ID, DiagonalFencesClient::new);
    }

    private static void registerEventInvokers() {
        ForgeEventInvokerRegistry.INSTANCE.register(ModelEvents.ModifyUnbakedModel.class, ModelEvent.ModifyBakingResult.class, (ModelEvents.ModifyUnbakedModel callback, ModelEvent.ModifyBakingResult evt) -> {
            Stopwatch stopwatch = Stopwatch.createStarted();
            ModelBakery modelBakery = evt.getModelBakery();
            Map<ResourceLocation, UnbakedModel> additionalModels = Maps.newHashMap();
            Function<ResourceLocation, UnbakedModel> modelGetter = (ResourceLocation resourceLocation) -> {
                if (additionalModels.containsKey(resourceLocation)) {
                    return additionalModels.get(resourceLocation);
                } else {
                    return modelBakery.getModel(resourceLocation);
                }
            };
            Multimap<ResourceLocation, Material> missingTextures = HashMultimap.create();
            Map<ForgeModelBakerImpl.ModelBakingKey, BakedModel> bakedCache = Maps.newIdentityHashMap();
            Map<UnbakedModel, UnbakedModel> modelCache = Maps.newIdentityHashMap();
            for (Map.Entry<ResourceLocation, BakedModel> entry : evt.getModels().entrySet()) {
                ResourceLocation resourceLocation = entry.getKey();
                UnbakedModel model = modelBakery.getModel(resourceLocation);
                UnbakedModel cachedModel = modelCache.getOrDefault(model, null);
                EventResultHolder<UnbakedModel> result = callback.onModifyUnbakedModel(resourceLocation, model, modelGetter, additionalModels::put, cachedModel);
                if (result.isInterrupt()) {
                    UnbakedModel unbakedModel = result.getInterrupt().get();
                    if (cachedModel != unbakedModel) {
                        additionalModels.put(resourceLocation, unbakedModel);
                        modelCache.put(model, unbakedModel);
                    }
                }
                if (modelCache.containsKey(model)) {
                    ForgeModelBakerImpl modelBaker = new ForgeModelBakerImpl(resourceLocation, bakedCache, modelGetter, missingTextures::put);
                    entry.setValue(modelBaker.bake(modelCache.get(model), resourceLocation));
                }
            }
            missingTextures.asMap().forEach((ResourceLocation resourceLocation, Collection<Material> materials) -> {
                PuzzlesLib.LOGGER.warn("Missing textures in model {}:\n{}", resourceLocation, materials.stream().sorted(Material.COMPARATOR).map((material) -> {
                    return "    " + material.atlasLocation() + ":" + material.texture();
                }).collect(Collectors.joining("\n")));
            });
            PuzzlesLib.LOGGER.info("Modifying unbaked models took {} millisecond(s)", stopwatch.stop().elapsed().toMillis());
        });
        ForgeEventInvokerRegistry.INSTANCE.register(ModelEvents.ModifyBakedModel.class, ModelEvent.ModifyBakingResult.class, (ModelEvents.ModifyBakedModel callback, ModelEvent.ModifyBakingResult evt) -> {
            Stopwatch stopwatch = Stopwatch.createStarted();
            Map<ResourceLocation, BakedModel> models = evt.getModels();
            Map<ResourceLocation, BakedModel> additionalModels = Maps.newLinkedHashMap();
            Multimap<ResourceLocation, Material> missingTextures = HashMultimap.create();
            Map<ForgeModelBakerImpl.ModelBakingKey, BakedModel> bakedCache = Maps.newIdentityHashMap();
            BakedModel missingModel = models.get(ModelBakery.MISSING_MODEL_LOCATION);
            Objects.requireNonNull(missingModel, "missing model is null");
            for (Map.Entry<ResourceLocation, BakedModel> entry : models.entrySet()) {
                EventResultHolder<BakedModel> result = callback.onModifyBakedModel(entry.getKey(), entry.getValue(), () -> {
                    return new ForgeModelBakerImpl(entry.getKey(), bakedCache, evt.getModelBakery()::getModel, missingTextures::put);
                }, (ResourceLocation resourceLocation) -> {
                    if (additionalModels.containsKey(resourceLocation)) {
                        return additionalModels.get(resourceLocation);
                    } else {
                        return models.getOrDefault(resourceLocation, missingModel);
                    }
                }, additionalModels::put);
                result.getInterrupt().ifPresent(entry::setValue);
            }
            additionalModels.forEach(models::putIfAbsent);
            missingTextures.asMap().forEach((ResourceLocation resourceLocation, Collection<Material> materials) -> {
                PuzzlesLib.LOGGER.warn("Missing textures in model {}:\n{}", resourceLocation, materials.stream().sorted(Material.COMPARATOR).map((material) -> {
                    return "    " + material.atlasLocation() + ":" + material.texture();
                }).collect(Collectors.joining("\n")));
            });
            PuzzlesLib.LOGGER.info("Modifying baked models took {} millisecond(s)", stopwatch.stop().elapsed().toMillis());
        });
        ForgeEventInvokerRegistry.INSTANCE.register(ModelEvents.AdditionalBakedModel.class, ModelEvent.ModifyBakingResult.class, (ModelEvents.AdditionalBakedModel callback, ModelEvent.ModifyBakingResult evt) -> {
            Stopwatch stopwatch = Stopwatch.createStarted();
            Map<ResourceLocation, BakedModel> models = evt.getModels();
            Multimap<ResourceLocation, Material> missingTextures = HashMultimap.create();
            Map<ForgeModelBakerImpl.ModelBakingKey, BakedModel> bakedCache = Maps.newIdentityHashMap();
            BakedModel missingModel = models.get(ModelBakery.MISSING_MODEL_LOCATION);
            Objects.requireNonNull(missingModel, "missing model is null");
            callback.onAdditionalBakedModel(models::putIfAbsent, (ResourceLocation resourceLocation) -> {
                return models.getOrDefault(resourceLocation, missingModel);
            }, () -> {
                // just use a dummy model, we cut this out when printing missing textures to the log
                return new ForgeModelBakerImpl(ModelBakery.MISSING_MODEL_LOCATION, bakedCache, evt.getModelBakery()::getModel, missingTextures::put);
            });
            missingTextures.asMap().forEach((ResourceLocation resourceLocation, Collection<Material> materials) -> {
                PuzzlesLib.LOGGER.warn("Missing textures:\n{}", materials.stream().sorted(Material.COMPARATOR).map((material) -> {
                    return "    " + material.atlasLocation() + ":" + material.texture();
                }).collect(Collectors.joining("\n")));
            });
            PuzzlesLib.LOGGER.info("Adding additional baked models took {} millisecond(s)", stopwatch.stop().elapsed().toMillis());
        });
    }
}
