package fuzs.diagonalblocks.client.handler;

import com.google.common.base.Suppliers;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import net.minecraft.Util;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.ModelManager;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

public class DiagonalModelHandler {
    private static final Supplier<Map<DiagonalBlockType, Map<ResourceLocation, Block>>> BASE_BLOCKS_BY_DIAGONAL_LOCATION;
    private static final Map<ResourceLocation, UnbakedModel> UNBAKED_MODEL_CACHE = Maps.newConcurrentMap();
    private static final Set<ResourceLocation> REPORTED_BLOCKS = Sets.newHashSet();

    static {
        BASE_BLOCKS_BY_DIAGONAL_LOCATION = Suppliers.memoize(() -> DiagonalBlockType.TYPES.stream().collect(ImmutableMap.toImmutableMap(Function.identity(), type -> {
            return type.getBlockConversions().entrySet().stream()
                    .map(entry -> Map.entry(BuiltInRegistries.BLOCK.getKey(entry.getValue()), entry.getKey()))
                    .collect(Util.toMap());
        })));
    }

    public static EventResultHolder<UnbakedModel> onModifyUnbakedModel(ResourceLocation modelLocation, Supplier<UnbakedModel> unbakedModel, Function<ResourceLocation, UnbakedModel> modelGetter, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        // ignore the unbaked model, it's just a dummy for preventing the model bakery from logging a missing model
        ResourceLocation resourceLocation = new ResourceLocation(modelLocation.getNamespace(), modelLocation.getPath());
        if (UNBAKED_MODEL_CACHE.containsKey(resourceLocation)) return EventResultHolder.interrupt(UNBAKED_MODEL_CACHE.get(resourceLocation));
        for (Map.Entry<DiagonalBlockType, Map<ResourceLocation, Block>> entry : BASE_BLOCKS_BY_DIAGONAL_LOCATION.get().entrySet()) {
            Block baseBlock = entry.getValue().get(resourceLocation);
            if (baseBlock != null) {
                Block diagonalBlock = entry.getKey().getBlockConversions().get(baseBlock);
                Objects.requireNonNull(diagonalBlock, "diagonal block is null");
                MultiPartTranslator translator = MultiPartTranslator.get(entry.getKey());
                // the unbaked model is the same for every possible block state for a block, so it's good enough to just pick out the 'any' base state
                ModelResourceLocation modelResourceLocation = translator.convertAnyBlockState(diagonalBlock, baseBlock);
                if (modelGetter.apply(modelResourceLocation) instanceof MultiPart multiPart) {
                    UnbakedModel newModel = translator.apply(diagonalBlock, multiPart, modelAdder);
                    UNBAKED_MODEL_CACHE.put(resourceLocation, newModel);
                    return EventResultHolder.interrupt(newModel);
                }
                if (REPORTED_BLOCKS.add(resourceLocation)) {
                    DiagonalBlocks.LOGGER.warn("Block '{}' is not using multipart model, diagonal connections will not be visible!", resourceLocation);
                }
                break;
            }
        }
        return EventResultHolder.pass();
    }

    public static void onAfterModelLoading(Supplier<ModelManager> modelManager) {
        UNBAKED_MODEL_CACHE.clear();
    }
}
