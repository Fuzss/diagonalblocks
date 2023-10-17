package fuzs.diagonalfences.client;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.model.MultipartAppender;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import fuzs.puzzleslib.api.event.v1.core.ForgeEventInvokerRegistry;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ModelEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;

import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.stream.Collectors;

@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
public class DiagonalFencesForgeClient {
    public static WeakReference<Map<ResourceLocation, AtlasSet.StitchResult>> atlasPreparationsReference = new WeakReference<>(null);

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        ClientModConstructor.construct(DiagonalFences.MOD_ID, DiagonalFencesClient::new);
        registerEventInvokers();
    }

    private static void registerEventInvokers() {
        ForgeEventInvokerRegistry.INSTANCE.register(ModelEventsV2.ModifyUnbakedModel.class, ModelEvent.ModifyBakingResult.class, (ModelEventsV2.ModifyUnbakedModel callback, ModelEvent.ModifyBakingResult evt) -> {
            Map<ResourceLocation, UnbakedModel> additionalUnbakedModels = Maps.newHashMap();
            Multimap<ResourceLocation, Material> missingTextures = HashMultimap.create();
            Map<BakedModel, BakedModel> bakedModels = Maps.newIdentityHashMap();
            ModelBakery modelBakery = evt.getModelBakery();
            for (Map.Entry<ResourceLocation, BakedModel> entry : evt.getModels().entrySet()) {
                BakedModel bakedModel = entry.getValue();
                if (!bakedModels.containsKey(bakedModel)) {
                    ResourceLocation resourceLocation = entry.getKey();
                    EventResultHolder<UnbakedModel> result = callback.onModifyUnbakedModel(resourceLocation, () -> modelBakery.getModel(resourceLocation), modelBakery::getModel, additionalUnbakedModels::put);
                    if (result.isInterrupt()) {
                        additionalUnbakedModels.put(resourceLocation, result.getInterrupt().get());
                        bakedModels.put(bakedModel, new ForgeModelBakerImpl(resourceLocation, modelBakery::getModel, additionalUnbakedModels::get, missingTextures::put).bake(resourceLocation, BlockModelRotation.X0_Y0));
                    }
                }
                if (bakedModels.containsKey(bakedModel)) {
                    entry.setValue(bakedModels.get(bakedModel));
                }
            }
            missingTextures.asMap().forEach((resourceLocation, materials) -> {
                DiagonalFences.LOGGER.warn("Missing textures in model {}:\n{}", resourceLocation, materials.stream().sorted(Material.COMPARATOR).map((material) -> {
                    return "    " + material.atlasLocation() + ":" + material.texture();
                }).collect(Collectors.joining("\n")));
            });
        });
    }

    public static void onModifyBakingResult(final ModelEvent.ModifyBakingResult evt) {
        Map<? extends ResourceLocation, Block> blocks = BuiltInRegistries.BLOCK.stream()
                .filter(block -> block instanceof FenceBlock || block instanceof IronBarsBlock)
                .filter(block -> block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties())
                .flatMap(block -> block.getStateDefinition().getPossibleStates().stream())
                .collect(Collectors.toUnmodifiableMap(BlockModelShaper::stateToModelLocation, BlockBehaviour.BlockStateBase::getBlock));
        Map<BakedModel, BakedModel> bakedModels = Maps.newIdentityHashMap();
        Map<ResourceLocation, UnbakedModel> additionalUnbakedModels = Maps.newHashMap();
        Multimap<ResourceLocation, Material> missingTextures = HashMultimap.create();
        for (Map.Entry<ResourceLocation, BakedModel> entry : evt.getModels().entrySet()) {
            if (blocks.containsKey(entry.getKey())) {
                entry.setValue(bakedModels.computeIfAbsent(entry.getValue(), bakedModel -> {
                    UnbakedModel model = evt.getModelBakery().getModel(entry.getKey());
                    Block block = blocks.get(entry.getKey());
                    if (model instanceof MultiPart multiPart) {
                        MultipartAppender.appendDiagonalSelectors(additionalUnbakedModels::put, multiPart, block instanceof IronBarsBlock);
                    } else {
                        DiagonalFences.LOGGER.warn("Block '{}' is not using multipart models, diagonal connections will not be visible!", block);
                    }
                    return new ForgeModelBakerImpl(entry.getKey(), evt.getModelBakery()::getModel, additionalUnbakedModels::get, missingTextures::put).bake(entry.getKey(), BlockModelRotation.X0_Y0);
                }));
            }
        }
        missingTextures.asMap().forEach((resourceLocation, materials) -> {
            DiagonalFences.LOGGER.warn("Missing textures in model {}:\n{}", resourceLocation, materials.stream().sorted(Material.COMPARATOR).map((material) -> {
                return "    " + material.atlasLocation() + ":" + material.texture();
            }).collect(Collectors.joining("\n")));
        });
    }
}
