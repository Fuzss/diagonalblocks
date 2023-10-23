package fuzs.diagonalfences.client.handler;

import com.google.common.base.Suppliers;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.client.util.MultiPartTranslator;
import fuzs.diagonalfences.client.util.MultipartAppender;
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
import java.util.stream.Stream;

public class WallModelHandler {
    private static final Map<DiagonalBlockType, MultiPartTranslator> MULTI_PART_TRANSLATORS;
    private static final Supplier<Map<DiagonalBlockType, Map<ResourceLocation, ResourceLocation>>> MODEL_LOCATION_CONVERSIONS;
    private static final Set<ResourceLocation> REPORTED_BLOCKS = Sets.newHashSet();

    static {
        Map<DiagonalBlockType, MultiPartTranslator> map = Map.of(DiagonalBlockType.FENCES, MultiPartTranslator.IDENTITY, DiagonalBlockType.WINDOWS, MultiPartTranslator.IDENTITY, DiagonalBlockType.WALLS, MultiPartTranslator.WALLS);
        MULTI_PART_TRANSLATORS = Maps.immutableEnumMap(map);
        MODEL_LOCATION_CONVERSIONS = Suppliers.memoize(() -> Stream.of(DiagonalBlockType.values()).collect(Maps.<DiagonalBlockType, DiagonalBlockType, Map<ResourceLocation, ResourceLocation>>toImmutableEnumMap(Function.identity(), type -> {
            return type.getBlockStateConversions().entrySet().stream().map(entry -> Map.entry(BlockModelShaper.stateToModelLocation(entry.getKey()), BlockModelShaper.stateToModelLocation(entry.getValue()))).collect(Util.toMap());
        })));
    }

    public static EventResultHolder<UnbakedModel> onModifyUnbakedModel(ResourceLocation modelLocation, UnbakedModel unbakedModel, Function<ResourceLocation, UnbakedModel> modelGetter, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        for (Map.Entry<DiagonalBlockType, Map<ResourceLocation, ResourceLocation>> entry : MODEL_LOCATION_CONVERSIONS.get().entrySet()) {
            ResourceLocation resourceLocation = entry.getValue().get(modelLocation);
            if (resourceLocation != null) {
                if (modelGetter.apply(resourceLocation) instanceof MultiPart multiPart) {
                    MultiPart newMultiPart = MULTI_PART_TRANSLATORS.get(entry.getKey()).apply(modelLocation, unbakedModel, multiPart);
                    newMultiPart = MultipartAppender.appendDiagonalSelectors(modelAdder, newMultiPart, entry.getKey() == DiagonalBlockType.WINDOWS);
                    return EventResultHolder.interrupt(newMultiPart);
                }
                modelLocation = new ResourceLocation(modelLocation.getNamespace(), modelLocation.getPath());
                if (REPORTED_BLOCKS.add(modelLocation)) {
                    DiagonalFences.LOGGER.warn("Block '{}' is not using multipart model, diagonal connections will not be visible!", modelLocation);
                }
            }
        }
        return EventResultHolder.pass();
    }
}
