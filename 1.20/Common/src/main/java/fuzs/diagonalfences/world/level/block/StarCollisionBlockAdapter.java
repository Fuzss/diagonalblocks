package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.api.v2.DiagonalBlockV2;
import net.minecraft.world.level.block.state.BlockState;

/**
 * An adapter for legacy methods from the old mixin implementation.
 */
public interface StarCollisionBlockAdapter extends StarCollisionBlock, DiagonalBlockV2 {

    @Override
    default boolean attachesTo(BlockState blockState, BlockState neighboringBlockState) {
        return neighboringBlockState.getBlock() instanceof DiagonalBlockV2 diagonalBlock && diagonalBlock.getType() == this.getType();
    }
}
