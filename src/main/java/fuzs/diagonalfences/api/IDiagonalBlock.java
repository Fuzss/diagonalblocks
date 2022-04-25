package fuzs.diagonalfences.api;

import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;

/**
 * mainly contains four new block states for diagonal directions
 */
public interface IDiagonalBlock {

    BooleanProperty NORTH_EAST = BooleanProperty.create("north_east");
    BooleanProperty SOUTH_EAST = BooleanProperty.create("south_east");
    BooleanProperty SOUTH_WEST = BooleanProperty.create("south_west");
    BooleanProperty NORTH_WEST = BooleanProperty.create("north_west");

    /**
     * @return have diagonal properties successfully been applied to this block
     */
    boolean hasProperties();

    /**
     * @return is this block not blacklisted via a block tag
     */
    boolean canConnectDiagonally();

    /**
     * @param blockstate other block
     * @return is a diagonal connection between both blocks allowed
     */
    boolean canConnectDiagonally(BlockState blockstate);

}
