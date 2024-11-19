package fuzs.diagonalblocks.api.v2.client;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.client.resources.model.MultipartAppender;
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

    protected final DiagonalBlockType type;

    protected MultiPartTranslator(DiagonalBlockType type) {
        this.type = type;
    }

    public static void register(DiagonalBlockType diagonalBlockType, MultiPartTranslator translator) {
        TRANSLATORS.put(diagonalBlockType, translator);
    }

    public static MultiPartTranslator get(DiagonalBlockType diagonalBlockType) {
        return TRANSLATORS.computeIfAbsent(diagonalBlockType, MultiPartTranslator::new);
    }

    private BlockState convertBlockState(StateDefinition<Block, BlockState> newStateDefinition, BlockState oldBlockState) {
        BlockState newBlockState = newStateDefinition.any();
        for (Map.Entry<Property<?>, Comparable<?>> entry : oldBlockState.getValues().entrySet()) {
            newBlockState = this.setBlockStateValue(entry.getKey(), entry.getValue(), newStateDefinition::getProperty,
                    newBlockState
            );
        }
        return newBlockState;
    }

    private <T extends Comparable<T>, V extends T> BlockState setBlockStateValue(Property<?> oldProperty, Comparable<?> oldValue, Function<String, @Nullable Property<?>> propertyGetter, BlockState blockState) {
        Property<?> newProperty = propertyGetter.apply(oldProperty.getName());
        if (newProperty != null) {
            Comparable<?> newValue = this.getNewPropertyValue(oldProperty, newProperty, oldValue);
            return blockState.setValue((Property<T>) newProperty, (V) newValue);
        } else {
            return blockState;
        }
    }

    protected Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
        return oldValue;
    }

    public MultiPart.Definition apply(MultiPart.Definition baseBlockModel, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        return this.applyAdditionalSelectors(modelAdder, this.getModelFromBase(baseBlockModel));
    }

    protected MultiPart.Definition getModelFromBase(MultiPart.Definition multiPart) {
        List<Selector> selectors = Lists.newArrayList(multiPart.selectors());
        return new MultiPart.Definition(selectors);
    }

    protected MultiPart.Definition applyAdditionalSelectors(BiConsumer<ResourceLocation, UnbakedModel> modelAdder, MultiPart.Definition multiPart) {
        return MultipartAppender.appendDiagonalSelectors(modelAdder, multiPart, false);
    }

    public boolean allowBaseModelAsFallback() {
        return true;
    }

    @Override
    public String toString() {
        return "MultiPartTranslator[" + this.type + "]";
    }
}
