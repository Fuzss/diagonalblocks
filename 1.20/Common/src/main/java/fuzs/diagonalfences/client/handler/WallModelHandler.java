package fuzs.diagonalfences.client.handler;

import com.google.common.base.Suppliers;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.client.util.MultipartAppender;
import fuzs.diagonalfences.mixin.client.accessor.KeyValueConditionAccessor;
import fuzs.diagonalfences.mixin.client.accessor.MultiPartAccessor;
import fuzs.diagonalfences.mixin.client.accessor.SelectorAccessor;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.properties.WallSide;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class WallModelHandler {
    private static final Supplier<Map<? extends ResourceLocation, ? extends ResourceLocation>> WALL_MODEL_LOCATIONS = Suppliers.memoize(() -> {
        Map<BlockState, BlockState> blockStates = Maps.newHashMap();
        for (Map.Entry<Block, Block> e1 : DiagonalBlockType.WALLS.getConversions().entrySet()) {
            for (BlockState possibleState : e1.getValue().getStateDefinition().getPossibleStates()) {
                StateDefinition<Block, BlockState> stateDefinition = e1.getKey().getStateDefinition();
                BlockState blockState = stateDefinition.any();
                for (Map.Entry<Property<?>, Comparable<?>> e2 : possibleState.getValues().entrySet()) {
                    blockState = setBlockStateValue(e2.getKey(), e2.getValue(), stateDefinition::getProperty, blockState);
                }
                blockStates.put(possibleState, blockState);
            }
        }
        return blockStates.entrySet().stream().collect(Collectors.toUnmodifiableMap(t -> BlockModelShaper.stateToModelLocation(t.getKey()), t -> BlockModelShaper.stateToModelLocation(t.getValue())));
    });

    private static <T extends Comparable<T>, V extends T> BlockState setBlockStateValue(Property<?> property, Comparable<?> value, Function<String, @Nullable Property<?>> propertyGetter, BlockState blockState) {
        Property<?> newProperty = propertyGetter.apply(property.getName());
        if (newProperty != null) {
            Comparable<?> newValue;
            if (newProperty.getValueClass() == WallSide.class) {
                newValue = (Boolean) value ? WallSide.LOW : WallSide.NONE;
            } else {
                newValue = value;
            }
            return blockState.setValue((Property<T>) newProperty, (V) newValue);
        }
        return blockState;
    }

    public static EventResultHolder<UnbakedModel> onModifyUnbakedModel(ResourceLocation modelLocation, UnbakedModel unbakedModel, Function<ResourceLocation, UnbakedModel> modelGetter, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        ResourceLocation resourceLocation = WALL_MODEL_LOCATIONS.get().get(modelLocation);
        if (resourceLocation != null) {
            if (modelGetter.apply(resourceLocation) instanceof MultiPart multiPart) {
                List<Selector> selectors = Lists.newArrayList(multiPart.getSelectors());
                ListIterator<Selector> iterator = selectors.listIterator();
                while (iterator.hasNext()) {
                    Selector selector = iterator.next();
                    if (((SelectorAccessor) selector).diagonalfences$getCondition() instanceof KeyValueCondition keyValueCondition) {
                        String value = ((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getValue();
                        if (value.equals(WallSide.LOW.toString())) {
                            value = Boolean.TRUE.toString();
                        } else if (value.equals(WallSide.NONE.toString())) {
                            value = Boolean.FALSE.toString();
                        } else if (value.equals(WallSide.TALL.toString())) {
                            value = null;
                        } else {
                            continue;
                        }
                        if (value != null) {
                            String key = ((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getKey();
                            iterator.set(new Selector(new KeyValueCondition(key, value), selector.getVariant()));
                        } else {
                            iterator.remove();
                        }
                    }
                }
                // we use a placeholder model that provided via a runtime data generator, so the model bakery doesn't log a missing model
                // also the generated placeholder purposefully uses multipart, so we can reuse the stored state definition
                if (!(unbakedModel instanceof MultiPart)) throw new IllegalArgumentException("invalid model for diagonal block: " + modelLocation);
                MultiPart newMultiPart = new MultiPart(((MultiPartAccessor) unbakedModel).diagonalfences$getDefinition(), selectors);
                newMultiPart = MultipartAppender.appendDiagonalSelectors(modelAdder, newMultiPart, false);
                return EventResultHolder.interrupt(newMultiPart);
            }
        }
        return EventResultHolder.pass();
    }
}
