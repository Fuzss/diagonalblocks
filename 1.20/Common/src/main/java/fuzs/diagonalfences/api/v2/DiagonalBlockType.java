package fuzs.diagonalfences.api.v2;

import com.google.common.collect.BiMap;
import com.google.common.collect.Sets;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Map;
import java.util.Set;

public interface DiagonalBlockType {
    Set<DiagonalBlockType> TYPES = Sets.newConcurrentHashSet();

    static void register(DiagonalBlockType diagonalBlockType) {
        if (TYPES.contains(diagonalBlockType) || !TYPES.add(diagonalBlockType)) {
            throw new IllegalStateException("duplicate diagonal block type '%s'".formatted(diagonalBlockType));
        }
    }

    TagKey<Block> getBlacklistTagKey();

    boolean isTarget(Block block);

    Block makeDiagonalBlock(Block block);

    BiMap<Block, Block> getConversions();

    Map<BlockState, BlockState> getBlockStateConversions();
}
