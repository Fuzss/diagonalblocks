package fuzs.diagonalblocks.init;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.puzzleslib.api.init.v3.tags.BoundTagFactory;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;

public class ModRegistry {
    static final BoundTagFactory TAGS = BoundTagFactory.make(DiagonalBlocks.MOD_ID);
    public static final TagKey<Block> NEVER_BLOCKS_DIAGONAL_CONNECTIONS_BLOCK_TAG = TAGS.registerBlockTag(
            "never_blocks_diagonal_connections");

    public static void touch() {
        // NO-OP
    }
}
