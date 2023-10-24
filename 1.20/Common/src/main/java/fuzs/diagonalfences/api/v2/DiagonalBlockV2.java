package fuzs.diagonalfences.api.v2;

import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.world.level.block.LegacyDiagonalBlockAdapter;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;

public interface DiagonalBlockV2 extends LegacyDiagonalBlockAdapter {
    BooleanProperty NORTH = CrossCollisionBlock.NORTH;
    BooleanProperty EAST = CrossCollisionBlock.EAST;
    BooleanProperty SOUTH = CrossCollisionBlock.SOUTH;
    BooleanProperty WEST = CrossCollisionBlock.WEST;
    BooleanProperty NORTH_EAST = DiagonalBlock.NORTH_EAST;
    BooleanProperty SOUTH_EAST = DiagonalBlock.SOUTH_EAST;
    BooleanProperty SOUTH_WEST = DiagonalBlock.SOUTH_WEST;
    BooleanProperty NORTH_WEST = DiagonalBlock.NORTH_WEST;

    DiagonalBlockType getType();

    boolean attachesDirectlyTo(BlockState blockState, boolean isSideSolid, Direction direction);

    boolean attachesDiagonallyTo(BlockState blockState);
}
