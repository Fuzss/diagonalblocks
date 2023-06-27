package fuzs.diagonalfences.api.world.level.block;

import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;

/**
 * mainly contains four new block states for diagonal directions
 */
public interface DiagonalBlock {
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
    boolean supportsDiagonalConnections();

    /**
     * @param neighborState neighbor block state to check if it can connect to me diagonally
     * @param neighborDirectionToMe my direction from the neighbor blocks point of view
     * @return is a diagonal connection between both blocks allowed
     */
    boolean canConnectToMe(BlockState neighborState, EightWayDirection neighborDirectionToMe);
}
