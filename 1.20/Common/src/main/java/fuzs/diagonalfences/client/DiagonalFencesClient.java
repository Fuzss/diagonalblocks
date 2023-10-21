package fuzs.diagonalfences.client;

import com.google.common.base.Suppliers;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.client.data.DynamicModelProvider;
import fuzs.diagonalfences.client.handler.FenceModelHandler;
import fuzs.diagonalfences.handler.WallBlockHandler;
import fuzs.diagonalfences.mixin.client.accessor.KeyValueConditionAccessor;
import fuzs.diagonalfences.mixin.client.accessor.MultiPartAccessor;
import fuzs.diagonalfences.mixin.client.accessor.SelectorAccessor;
import fuzs.diagonalfences.world.level.block.DiagonalWallBlock;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.core.v1.context.PackRepositorySourcesContext;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import fuzs.puzzleslib.api.resources.v1.DynamicPackResources;
import fuzs.puzzleslib.api.resources.v1.PackResourcesHelper;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.WallBlock;
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

public class DiagonalFencesClient implements ClientModConstructor {
    private static final Supplier<Map<? extends ResourceLocation, ? extends ResourceLocation>> MODEL_LOCATIONS = Suppliers.memoize(() -> {
        Map<BlockState, BlockState> blockStates = Maps.newHashMap();
        for (Map.Entry<WallBlock, DiagonalWallBlock> e1 : WallBlockHandler.getWallBlocks().entrySet()) {
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

    @Override
    public void onConstructMod() {
        registerHandlers();
    }

    private static void registerHandlers() {
        ModelEventsV2.MODIFY_UNBAKED_MODEL.register(FenceModelHandler::onModifyUnbakedModel);
        ModelEventsV2.MODIFY_UNBAKED_MODEL.register((ResourceLocation modelLocation, UnbakedModel unbakedModel, Function<ResourceLocation, UnbakedModel> modelGetter, BiConsumer<ResourceLocation, UnbakedModel> modelAdder, @Nullable UnbakedModel cachedModel) -> {
            if (cachedModel != null) return EventResultHolder.pass();
            ResourceLocation resourceLocation = MODEL_LOCATIONS.get().get(modelLocation);
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
                    selectors.removeIf(selector -> {
                        if (((SelectorAccessor) selector).diagonalfences$getCondition() instanceof KeyValueCondition keyValueCondition) {
                            return ((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getValue().equals("tall");
                        }
                        return false;
                    });
                    return EventResultHolder.interrupt(new MultiPart(((MultiPartAccessor) unbakedModel).diagonalfences$getDefinition(), selectors));
                }
            }
            return EventResultHolder.pass();
        });
    }

    @Override
    public void onAddResourcePackFinders(PackRepositorySourcesContext context) {
        context.addRepositorySource(PackResourcesHelper.buildClientPack(DiagonalFences.id("dynamic_walls"), DynamicPackResources.create(DynamicModelProvider::new), true));
    }
}
