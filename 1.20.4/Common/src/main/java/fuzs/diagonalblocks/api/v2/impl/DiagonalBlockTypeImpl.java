package fuzs.diagonalblocks.api.v2.impl;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Maps;
import fuzs.diagonalblocks.api.v2.DiagonalBlock;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.properties.Property;

import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

public class DiagonalBlockTypeImpl implements DiagonalBlockType {
    private static final UnaryOperator<Block> NULL_FACTORY = UnaryOperator.identity();

    private final BiMap<Block, Block> blocks = HashBiMap.create();
    private final BiMap<Block, Block> blocksView = Maps.unmodifiableBiMap(this.blocks);
    private final ResourceLocation name;
    private final Class<? extends Block> targetType;
    private final UnaryOperator<Block> factory;
    private final int blockPropertiesCount;
    private final TagKey<Block> blacklistTagKey;
    private final Map<ResourceLocation, UnaryOperator<Block>> factoryOverrides = Maps.newConcurrentMap();

    public DiagonalBlockTypeImpl(String name, Class<? extends Block> targetType, UnaryOperator<Block> factory, Property<?>... blockProperties) {
        this(name, targetType, factory, blockProperties.length);
    }

    public DiagonalBlockTypeImpl(String name, Class<? extends Block> targetType, UnaryOperator<Block> factory, int blockPropertiesCount) {
        name = name.toLowerCase(Locale.ROOT);
        this.name = new ResourceLocation("diagonal" + name, name);
        this.targetType = targetType;
        this.factory = factory;
        this.blockPropertiesCount = blockPropertiesCount;
        this.blacklistTagKey = TagKey.create(Registries.BLOCK, this.id("non_diagonal_" + name));
    }

    @Override
    public ResourceLocation id(String path) {
        return new ResourceLocation(this.name.getNamespace(), path);
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
            boolean isTarget = this.targetType.isInstance(block) && this.blockPropertiesCount == block.getStateDefinition().getProperties().size();
            return isTarget || this.factoryOverrides.containsKey(resourceLocation);
        }

        return false;
    }

    @Override
    public Block makeDiagonalBlock(ResourceLocation resourceLocation, Block block) {
        if (this.isTarget(resourceLocation, block)) {
            Block diagonalBlock = this.factoryOverrides.getOrDefault(resourceLocation, this.factory).apply(block);
            Objects.requireNonNull(diagonalBlock, "diagonal block for '%s' is null".formatted(resourceLocation));
            this.blocks.put(block, diagonalBlock);
            return diagonalBlock;
        } else {
            throw new IllegalArgumentException("%s is no target for %s".formatted(block, this));
        }
    }

    @Override
    public BiMap<Block, Block> getBlockConversions() {
        return this.blocksView;
    }

    @Override
    public void registerBlockFactory(ResourceLocation resourceLocation, UnaryOperator<Block> factory) {
        Objects.requireNonNull(resourceLocation, "resource location is null");
        Objects.requireNonNull(factory, "factory is null");
        this.factoryOverrides.put(resourceLocation, factory);
    }

    @Override
    public void registerDefaultBlockFactory(ResourceLocation resourceLocation) {
        Objects.requireNonNull(resourceLocation, "resource location is null");
        this.factoryOverrides.put(resourceLocation, this.factory);
    }

    @Override
    public void disableBlockFactory(ResourceLocation resourceLocation) {
        Objects.requireNonNull(resourceLocation, "resource location is null");
        this.factoryOverrides.put(resourceLocation, NULL_FACTORY);
    }
}
