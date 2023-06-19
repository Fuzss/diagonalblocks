package fuzs.diagonalfences.api;

import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import net.minecraft.world.level.block.state.properties.BooleanProperty;

/**
 * moved to proper location at {@link DiagonalBlock}
 */
@Deprecated(forRemoval = true)
public interface IDiagonalBlock extends DiagonalBlock {
    BooleanProperty NORTH_EAST = DiagonalBlock.NORTH_EAST;
    BooleanProperty SOUTH_EAST = DiagonalBlock.SOUTH_EAST;
    BooleanProperty SOUTH_WEST = DiagonalBlock.SOUTH_WEST;
    BooleanProperty NORTH_WEST = DiagonalBlock.NORTH_WEST;
}
