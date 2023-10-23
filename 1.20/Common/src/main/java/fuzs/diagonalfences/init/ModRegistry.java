package fuzs.diagonalfences.init;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.puzzleslib.api.init.v3.tags.BoundTagFactory;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;

public class ModRegistry {
    static final BoundTagFactory TAGS = BoundTagFactory.make(DiagonalFences.MOD_ID);
    public static final TagKey<Block> NON_DIAGONAL_FENCES_BLOCK_TAG = TAGS.registerBlockTag("non_diagonal_fences");
    public static final TagKey<Block> NON_DIAGONAL_PANES_BLOCK_TAG = TAGS.registerBlockTag("non_diagonal_panes");
    public static final TagKey<Block> NON_DIAGONAL_WALLS_BLOCK_TAG = TAGS.registerBlockTag("non_diagonal_walls");

    public static void touch() {

    }
}
