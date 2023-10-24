package fuzs.diagonalfences.api.v2;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.core.CommonAbstractions;
import net.minecraft.core.registries.Registries;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.WallBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.properties.WallSide;
import org.jetbrains.annotations.Nullable;

import java.util.Locale;
import java.util.Map;
import java.util.function.Function;
import java.util.function.UnaryOperator;

public enum DiagonalBlockType {
    FENCES(FenceBlock.class, CommonAbstractions.INSTANCE::getDiagonalFenceBlock),
    WINDOWS(IronBarsBlock.class, CommonAbstractions.INSTANCE::getDiagonalGlassPaneBlock),
    WALLS(WallBlock.class, CommonAbstractions.INSTANCE::getDiagonalWallBlock) {

        @Override
        Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
            if (newProperty.getValueClass() == WallSide.class) {
                return (Boolean) oldValue ? WallSide.LOW : WallSide.NONE;
            } else {
                return super.getNewPropertyValue(oldProperty, newProperty, oldValue);
            }
        }
    };

    private final BiMap<Block, Block> blocks = HashBiMap.create();
    private final Class<? extends Block> targetType;
    private final UnaryOperator<Block> factory;
    public final TagKey<Block> blacklistTagKey;

    DiagonalBlockType(Class<? extends Block> targetType, UnaryOperator<Block> factory) {
        this.targetType = targetType;
        this.factory = factory;
        this.blacklistTagKey = TagKey.create(Registries.BLOCK, DiagonalFences.id("non_diagonal_" + this));
    }

    @Override
    public String toString() {
        return this.name().toLowerCase(Locale.ROOT);
    }

    public boolean isTarget(Block block) {
        return !(block instanceof DiagonalBlock) && this.targetType.isInstance(block);
    }

    public Block makeDiagonalBlock(Block block) {
        if (this.isTarget(block)) {
            Block diagonalBlock = this.factory.apply(block);
            this.blocks.put(block, diagonalBlock);
            return diagonalBlock;
        } else {
            throw new IllegalArgumentException("%s is no target for %s".formatted(block, this));
        }
    }

    public BiMap<Block, Block> getConversions() {
        return Maps.unmodifiableBiMap(this.blocks);
    }

    public Map<BlockState, BlockState> getBlockStateConversions() {
        Map<BlockState, BlockState> blockStates = Maps.newHashMap();
        for (Map.Entry<Block, Block> e1 : this.blocks.entrySet()) {
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

    Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
        return oldValue;
    }
}
