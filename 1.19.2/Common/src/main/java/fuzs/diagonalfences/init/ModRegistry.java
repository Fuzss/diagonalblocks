package fuzs.diagonalfences.init;

import fuzs.diagonalfences.DiagonalFences;
import net.minecraft.core.Registry;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;

public class ModRegistry {
    public static final TagKey<Block> NON_DIAGONAL_FENCES_BLOCK_TAG = TagKey.create(Registry.BLOCK_REGISTRY, DiagonalFences.id("non_diagonal_fences"));
    public static final TagKey<Block> NON_DIAGONAL_PANES_BLOCK_TAG = TagKey.create(Registry.BLOCK_REGISTRY, DiagonalFences.id("non_diagonal_panes"));

    public static void touch() {

    }
}
