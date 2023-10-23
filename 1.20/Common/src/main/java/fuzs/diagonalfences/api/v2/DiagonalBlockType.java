package fuzs.diagonalfences.api.v2;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.core.CommonAbstractions;
import net.minecraft.core.registries.Registries;
import net.minecraft.tags.BlockTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.WallBlock;
import org.jetbrains.annotations.Nullable;

import java.util.Locale;
import java.util.function.UnaryOperator;

public enum DiagonalBlockType {
    FENCES(FenceBlock.class, null, BlockTags.MINEABLE_WITH_AXE), WINDOWS(IronBarsBlock.class, null, null), WALLS(WallBlock.class, CommonAbstractions.INSTANCE::getDiagonalWallBlock, BlockTags.MINEABLE_WITH_PICKAXE);

    private final BiMap<Block, Block> blocks = HashBiMap.create();
    private final Class<? extends Block> targetType;
    private final UnaryOperator<Block> factory;
    public final TagKey<Block> blacklistTagKey;
    @Nullable
    public final TagKey<Block> miningTagKey;

    DiagonalBlockType(Class<? extends Block> targetType, UnaryOperator<Block> factory, TagKey<Block> miningTagKey) {
        this.targetType = targetType;
        this.factory = factory;
        this.blacklistTagKey = TagKey.create(Registries.BLOCK, DiagonalFences.id("non_diagonal_" + this));
        this.miningTagKey = miningTagKey;
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
}
