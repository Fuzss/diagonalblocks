package fuzs.diagonalfences.api.v2;

import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.world.level.block.LegacyDiagonalBlockAdapter;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;

public interface DiagonalBlockV2 extends LegacyDiagonalBlockAdapter {
    BooleanProperty NORTH_EAST = DiagonalBlock.NORTH_EAST;
    BooleanProperty SOUTH_EAST = DiagonalBlock.SOUTH_EAST;
    BooleanProperty SOUTH_WEST = DiagonalBlock.SOUTH_WEST;
    BooleanProperty NORTH_WEST = DiagonalBlock.NORTH_WEST;

    DiagonalBlockType getType();

    boolean attachesTo(BlockState blockState, BlockState neighboringBlockState);
}
