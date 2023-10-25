package fuzs.diagonalblocks.client.handler;

import com.google.common.base.Suppliers;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Sets;
import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import net.minecraft.Util;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;

import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

public class DiagonalModelHandler {
    private static final Supplier<Map<DiagonalBlockType, Map<ResourceLocation, ResourceLocation>>> MODEL_LOCATION_CONVERSIONS;
    private static final Set<ResourceLocation> REPORTED_BLOCKS = Sets.newHashSet();

    static {
        MODEL_LOCATION_CONVERSIONS = Suppliers.memoize(() -> DiagonalBlockType.TYPES.stream().collect(ImmutableMap.toImmutableMap(Function.identity(), type -> {
            return MultiPartTranslator.get(type).getBlockStateConversions().entrySet().stream()
                    .map(entry -> Map.entry(BlockModelShaper.stateToModelLocation(entry.getKey()), BlockModelShaper.stateToModelLocation(entry.getValue())))
                    .collect(Util.toMap());
        })));
    }

    public static EventResultHolder<UnbakedModel> onModifyUnbakedModel(ResourceLocation modelLocation, UnbakedModel unbakedModel, Function<ResourceLocation, UnbakedModel> modelGetter, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        for (Map.Entry<DiagonalBlockType, Map<ResourceLocation, ResourceLocation>> entry : MODEL_LOCATION_CONVERSIONS.get().entrySet()) {
            ResourceLocation resourceLocation = entry.getValue().get(modelLocation);
            if (resourceLocation != null) {
                if (modelGetter.apply(resourceLocation) instanceof MultiPart multiPart) {
                    MultiPartTranslator translator = MultiPartTranslator.get(entry.getKey());
                    return EventResultHolder.interrupt(translator.apply(modelLocation, unbakedModel, multiPart, modelAdder));
                }
                modelLocation = new ResourceLocation(modelLocation.getNamespace(), modelLocation.getPath());
                if (REPORTED_BLOCKS.add(modelLocation)) {
                    DiagonalBlocks.LOGGER.warn("Block '{}' is not using multipart model, diagonal connections will not be visible!", modelLocation);
                }
            }
        }
        return EventResultHolder.pass();
    }
}
