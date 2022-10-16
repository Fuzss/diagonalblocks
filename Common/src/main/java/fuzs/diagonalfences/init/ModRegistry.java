package fuzs.diagonalfences.init;

import fuzs.diagonalfences.DiagonalFences;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;

public class ModRegistry {
    public static final TagKey<Block> NON_DIAGONAL_FENCES_TAG = TagKey.create(Registry.BLOCK_REGISTRY, new ResourceLocation(DiagonalFences.MOD_ID, "non_diagonal_fences"));
    public static final TagKey<Block> NON_DIAGONAL_FENCE_GATES_TAG = TagKey.create(Registry.BLOCK_REGISTRY, new ResourceLocation(DiagonalFences.MOD_ID, "non_diagonal_fence_gates"));
}
