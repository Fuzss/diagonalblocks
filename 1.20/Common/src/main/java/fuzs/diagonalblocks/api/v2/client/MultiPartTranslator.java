package fuzs.diagonalblocks.api.v2.client;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.client.resources.model.MultipartAppender;
import fuzs.diagonalblocks.mixin.client.accessor.MultiPartAccessor;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.Property;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class MultiPartTranslator {
    private static final Map<DiagonalBlockType, MultiPartTranslator> TRANSLATORS = Maps.newConcurrentMap();

    private final DiagonalBlockType type;

    protected MultiPartTranslator(DiagonalBlockType type) {
        this.type = type;
    }

    public static void register(DiagonalBlockType diagonalBlockType, MultiPartTranslator translator) {
        TRANSLATORS.put(diagonalBlockType, translator);
    }

    public static MultiPartTranslator get(DiagonalBlockType diagonalBlockType) {
        return TRANSLATORS.computeIfAbsent(diagonalBlockType, MultiPartTranslator::new);
    }

    public Map<BlockState, BlockState> getBlockStateConversions() {
        Map<BlockState, BlockState> blockStates = Maps.newHashMap();
        for (Map.Entry<Block, Block> e1 : this.type.getBlockConversions().entrySet()) {
            for (BlockState possibleState : e1.getValue().getStateDefinition().getPossibleStates()) {
                StateDefinition<Block, BlockState> stateDefinition = e1.getKey().getStateDefinition();
                BlockState blockState = stateDefinition.any();
                for (Map.Entry<Property<?>, Comparable<?>> e2 : possibleState.getValues().entrySet()) {
                    blockState = this.setBlockStateValue(e2.getKey(), e2.getValue(), stateDefinition::getProperty, blockState);
                }
                blockStates.put(possibleState, blockState);
            }
        }
        return blockStates;
    }

    private <T extends Comparable<T>, V extends T> BlockState setBlockStateValue(Property<?> oldProperty, Comparable<?> oldValue, Function<String, @Nullable Property<?>> propertyGetter, BlockState blockState) {
        Property<?> newProperty = propertyGetter.apply(oldProperty.getName());
        if (newProperty != null) {
            Comparable<?> newValue = this.getNewPropertyValue(oldProperty, newProperty, oldValue);
            return blockState.setValue((Property<T>) newProperty, (V) newValue);
        }
        return blockState;
    }

    protected Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
        return oldValue;
    }

    public MultiPart apply(ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, MultiPart baseBlockModel, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        return this.applyAdditionalSelectors(modelAdder, this.getModelFromBase(modelLocation, diagonalBlockModel, baseBlockModel));
    }

    protected MultiPart getModelFromBase(ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, MultiPart baseBlockModel) {
        return this.makeMultiPart(modelLocation, diagonalBlockModel, Lists.newArrayList(baseBlockModel.getSelectors()));
    }

    protected MultiPart makeMultiPart(ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, List<Selector> selectors) {
        // we use a placeholder model that is provided via a runtime data generator, so the model bakery doesn't log a missing model
        // also the generated placeholder purposefully uses multipart, so we can reuse the stored state definition
        if (!(diagonalBlockModel instanceof MultiPart)) {
            throw new IllegalArgumentException("invalid model for diagonal block '%s': '%s'".formatted(new ResourceLocation(modelLocation.getNamespace(), modelLocation.getPath()), diagonalBlockModel));
        }
        return new MultiPart(((MultiPartAccessor) diagonalBlockModel).diagonalfences$getDefinition(), selectors);
    }

    protected MultiPart applyAdditionalSelectors(BiConsumer<ResourceLocation, UnbakedModel> modelAdder, MultiPart multiPart) {
        return MultipartAppender.appendDiagonalSelectors(modelAdder, multiPart, false);
    }
}
