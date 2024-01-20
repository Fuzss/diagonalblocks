package fuzs.diagonalblocks.api.v2;

import fuzs.diagonalblocks.api.v2.impl.StarCollisionBlock;
import fuzs.diagonalblocks.api.v2.impl.StarShapeProvider;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;

/**
 * The very basic implementation for a block with diagonal block state properties.
 * <p>Most of the actual implementation is found in {@link StarCollisionBlock} and {@link StarShapeProvider}.
 */
public interface DiagonalBlock {
    BooleanProperty NORTH = CrossCollisionBlock.NORTH;
    BooleanProperty EAST = CrossCollisionBlock.EAST;
    BooleanProperty SOUTH = CrossCollisionBlock.SOUTH;
    BooleanProperty WEST = CrossCollisionBlock.WEST;
    BooleanProperty NORTH_EAST = BooleanProperty.create("north_east");
    BooleanProperty SOUTH_EAST = BooleanProperty.create("south_east");
    BooleanProperty SOUTH_WEST = BooleanProperty.create("south_west");
    BooleanProperty NORTH_WEST = BooleanProperty.create("north_west");

    /**
     * The type for this diagonal block, useful for determining if an attachment to another diagonal block can be made.
     *
     * @return the type for this diagonal block
     */
    DiagonalBlockType getType();

    /**
     * Checks if this block can connect to a direct neighbor in the horizontal plane.
     * Used for checking cardinal directions (north, east, south and east).
     * <p>This serves as a generified pass-through for a method usually already implemented on instances of {@link CrossCollisionBlock}, like {@link net.minecraft.world.level.block.FenceBlock#connectsTo(BlockState, boolean, Direction)}.
     *
     * @param blockState  the directly neighboring block state
     * @param isSideSolid is the neighbors side at <code>direction</code> solid
     * @param direction   the neighbors side this block is trying to attach to
     * @return can this block connect to the direct neighbor
     */
    boolean attachesDirectlyTo(BlockState blockState, boolean isSideSolid, Direction direction);

    /**
     * Checks if this block can connect to an indirect neighbor (another diagonal block) in the horizontal plane.
     * Use for checking intercardinal directions (north-east, south-east, south-west and north-west).
     * <p>The implementation is intended to check compatibility for connection purely on a block based evaluation, independently of the direction and block state properties.
     * <p>This is usually implemented to merely compare the result of {@link DiagonalBlock#getType()}.
     *
     * @param blockState the neighboring block state
     * @return can this block connect to the indirect neighbor
     */
    boolean attachesDiagonallyTo(BlockState blockState);
}
