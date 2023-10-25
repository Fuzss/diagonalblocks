package fuzs.diagonalfences.api.v2.impl;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;

import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.function.UnaryOperator;

public class DiagonalBlockTypeImpl implements DiagonalBlockType {
    private static final UnaryOperator<Block> NULL_FACTORY = UnaryOperator.identity();

    private final BiMap<Block, Block> blocks = HashBiMap.create();
    private final BiMap<Block, Block> blocksView = Maps.unmodifiableBiMap(this.blocks);
    private final ResourceLocation name;
    private final Class<? extends Block> targetType;
    private final UnaryOperator<Block> factory;
    private final TagKey<Block> blacklistTagKey;
    private final Map<ResourceLocation, UnaryOperator<Block>> factoryOverrides = Maps.newConcurrentMap();
    public DiagonalBlockTypeImpl(String name, Class<? extends Block> targetType, UnaryOperator<Block> factory) {
        name = name.toLowerCase(Locale.ROOT);
        this.name = new ResourceLocation("diagonal" + name, name);
        this.targetType = targetType;
        this.factory = factory;
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
        return !(block instanceof DiagonalBlock) && this.factoryOverrides.get(resourceLocation) != NULL_FACTORY && (this.targetType.isInstance(block) || this.factoryOverrides.containsKey(resourceLocation));
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
