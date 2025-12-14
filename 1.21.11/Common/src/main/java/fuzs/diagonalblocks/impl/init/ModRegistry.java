package fuzs.diagonalblocks.impl.init;

import fuzs.diagonalblocks.impl.DiagonalBlocks;
import fuzs.puzzleslib.api.init.v3.tags.TagFactory;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;

public class ModRegistry {
    static final TagFactory TAGS = TagFactory.make(DiagonalBlocks.MOD_ID);
    public static final TagKey<Block> NEVER_BLOCKS_DIAGONAL_CONNECTIONS_BLOCK_TAG = TAGS.registerBlockTag(
            "never_blocks_diagonal_connections");

    public static void bootstrap() {
        // NO-OP
    }
}
