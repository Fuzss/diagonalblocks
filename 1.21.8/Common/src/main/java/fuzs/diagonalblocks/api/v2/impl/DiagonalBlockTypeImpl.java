package fuzs.diagonalblocks.api.v2.impl;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Maps;
import fuzs.diagonalblocks.api.v2.DiagonalBlock;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.puzzleslib.api.core.v1.utility.ResourceLocationHelper;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.Property;

import java.util.IdentityHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class DiagonalBlockTypeImpl implements DiagonalBlockType {
    private static final Function<Block, Function<BlockBehaviour.Properties, Block>> NULL_FACTORY = (Block block) -> null;
    public static final Map<Block, Block> NON_DIAGONAL_TO_DIAGONAL_BLOCKS = new IdentityHashMap<>();
    public static final Map<BlockState, BlockState> NON_DIAGONAL_TO_DIAGONAL_BLOCK_STATES = new IdentityHashMap<>();
    public static final Map<BlockState, BlockState> DIAGONAL_TO_NON_DIAGONAL_BLOCK_STATES = new IdentityHashMap<>();

    private final BiMap<Block, Block> blocks = HashBiMap.create();
    private final BiMap<Block, Block> blocksView = Maps.unmodifiableBiMap(this.blocks);
    private final ResourceLocation name;
    private final Class<? extends Block> targetType;
    private final Function<Block, Function<BlockBehaviour.Properties, Block>> defaultBlockFactory;
    private final int blockPropertiesCount;
    private final TagKey<Block> blacklistTagKey;
    private final Map<ResourceLocation, Function<Block, Function<BlockBehaviour.Properties, Block>>> factoryOverrides = Maps.newConcurrentMap();

    public DiagonalBlockTypeImpl(String name, Class<? extends Block> targetType, Function<Block, Function<BlockBehaviour.Properties, Block>> defaultBlockFactory, Property<?>... blockProperties) {
        this(name, targetType, defaultBlockFactory, blockProperties.length);
    }

    DiagonalBlockTypeImpl(String name, Class<? extends Block> targetType, Function<Block, Function<BlockBehaviour.Properties, Block>> defaultBlockFactory, int blockPropertiesCount) {
        name = name.toLowerCase(Locale.ROOT);
        this.name = ResourceLocationHelper.fromNamespaceAndPath("diagonal" + name, name);
        this.targetType = targetType;
        this.defaultBlockFactory = defaultBlockFactory;
        this.blockPropertiesCount = blockPropertiesCount;
        this.blacklistTagKey = TagKey.create(Registries.BLOCK, this.id("non_diagonal_" + name));
    }

    @Override
    public ResourceLocation id(String path) {
        return ResourceLocationHelper.fromNamespaceAndPath(this.name.getNamespace(), path);
    }

    @Override
    public TagKey<Block> getBlacklistTagKey() {
        return this.blacklistTagKey;
    }

    @Override
    public String toString() {
        return this.name.toString();
    }

    @Override
    public boolean isTarget(ResourceLocation resourceLocation, Block block) {
        if (!(block instanceof DiagonalBlock) && this.factoryOverrides.get(resourceLocation) != NULL_FACTORY) {
            // check that block state properties count matches, we might otherwise run into issues with blocks that extend the base class and expect certain properties to be present
            // especially an issue when running BlockBehaviour.Properties::copy with behavior properties that depend on block state properties, such as BlockBehaviour.Properties::lightLevel
            // checking the count is enough, no need to compare actual properties
            boolean isTarget =
                    this.targetType.isInstance(block) && this.blockPropertiesCount == block.getStateDefinition()
                            .getProperties()
                            .size();
            return isTarget || this.factoryOverrides.containsKey(resourceLocation);
        }

        return false;
    }

    @Override
    public Block makeDiagonalBlock(ResourceLocation resourceLocation, Block block) {
        if (this.isTarget(resourceLocation, block)) {
            ResourceLocation id = this.id(resourceLocation.getNamespace() + "/" + resourceLocation.getPath());
            Function<BlockBehaviour.Properties, Block> blockFactory = this.factoryOverrides.getOrDefault(
                    resourceLocation,
                    this.defaultBlockFactory).apply(block);
            BlockBehaviour.Properties properties = BlockBehaviour.Properties.ofFullCopy(block)
                    .overrideLootTable(block.getLootTable())
                    .overrideDescription(block.getDescriptionId())
                    .setId(ResourceKey.create(Registries.BLOCK, id));
            Block diagonalBlock = blockFactory.apply(properties);
            Objects.requireNonNull(diagonalBlock, () -> "diagonal block for '%s' is null".formatted(resourceLocation));
            this.blocks.put(block, diagonalBlock);
            if (this.supportsOriginalBlockState()) {
                NON_DIAGONAL_TO_DIAGONAL_BLOCKS.put(block, diagonalBlock);
                this.copyAllBlockStateProperties(block, diagonalBlock, (BlockState key, BlockState value) -> {
                    // putIfAbsent is not a proper solution, there can be duplicates here for walls for the wall sides,
                    // then we want to make sure the low wall side is used
                    NON_DIAGONAL_TO_DIAGONAL_BLOCK_STATES.putIfAbsent(key, value);
                    DIAGONAL_TO_NON_DIAGONAL_BLOCK_STATES.putIfAbsent(value, key);
                });
            }
            return diagonalBlock;
        } else {
            throw new IllegalArgumentException("%s is no target for %s".formatted(block, this));
        }
    }

    private void copyAllBlockStateProperties(Block block, Block otherBlock, BiConsumer<BlockState, BlockState> blockStateConsumer) {
        for (BlockState blockState : block.getStateDefinition().getPossibleStates()) {
            blockStateConsumer.accept(blockState, this.copyBlockStateProperties(blockState, otherBlock));
        }
    }

    private <T extends Comparable<T>, V extends T> BlockState copyBlockStateProperties(BlockState blockState, Block block) {
        BlockState newBlockState = block.defaultBlockState();
        for (Map.Entry<Property<?>, Comparable<?>> entry : blockState.getValues().entrySet()) {
            Property<?> property = this.sanitizeProperty(entry.getKey());
            Comparable<?> value = this.sanitizePropertyValue(entry.getKey(), entry.getValue());
            newBlockState = newBlockState.trySetValue((Property<T>) property, (V) value);
        }
        return newBlockState;
    }

    protected Property<?> sanitizeProperty(Property<?> property) {
        return property;
    }

    protected Comparable<?> sanitizePropertyValue(Property<?> property, Comparable<?> value) {
        return value;
    }

    @Override
    public BiMap<Block, Block> getBlockConversions() {
        return this.blocksView;
    }

    @Override
    public void registerBlockFactory(ResourceLocation resourceLocation, Function<Block, Function<BlockBehaviour.Properties, Block>> factory) {
        Objects.requireNonNull(resourceLocation, "resource location is null");
        Objects.requireNonNull(factory, "factory is null");
        this.factoryOverrides.put(resourceLocation, factory);
    }

    @Override
    public void registerDefaultBlockFactory(ResourceLocation resourceLocation) {
        Objects.requireNonNull(resourceLocation, "resource location is null");
        this.factoryOverrides.put(resourceLocation, this.defaultBlockFactory);
    }

    @Override
    public void disableBlockFactory(ResourceLocation resourceLocation) {
        Objects.requireNonNull(resourceLocation, "resource location is null");
        this.factoryOverrides.put(resourceLocation, NULL_FACTORY);
    }
}
