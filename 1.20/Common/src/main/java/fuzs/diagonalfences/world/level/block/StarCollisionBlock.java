package fuzs.diagonalfences.world.level.block;

import com.google.common.collect.Maps;
import fuzs.diagonalfences.api.v2.DiagonalBlockV2;
import fuzs.diagonalfences.api.world.level.block.EightWayDirection;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BooleanProperty;

import java.util.Map;

public interface StarCollisionBlock extends DiagonalBlockV2, StarShapeProvider {
    Map<EightWayDirection, BooleanProperty> PROPERTY_BY_DIRECTION = Maps.immutableEnumMap(Map.of(EightWayDirection.NORTH, DiagonalBlockV2.NORTH, EightWayDirection.EAST, DiagonalBlockV2.EAST, EightWayDirection.SOUTH, DiagonalBlockV2.SOUTH, EightWayDirection.WEST, DiagonalBlockV2.WEST, EightWayDirection.NORTH_EAST, DiagonalBlockV2.NORTH_EAST, EightWayDirection.SOUTH_EAST, DiagonalBlockV2.SOUTH_EAST, EightWayDirection.SOUTH_WEST, DiagonalBlockV2.SOUTH_WEST, EightWayDirection.NORTH_WEST, DiagonalBlockV2.NORTH_WEST));

    @Override
    default boolean attachesDiagonallyTo(BlockState blockState) {
        return blockState.getBlock() instanceof DiagonalBlockV2 diagonalBlock && diagonalBlock.getType() == this.getType();
    }

    /**
     * sets default states for inter-cardinal properties
     * @param defaultState already modified default state obtained from {@link Block#defaultBlockState()}
     * @return state after setting inter-cardinal block states
     */
    default BlockState addDefaultStates(BlockState defaultState) {
        return defaultState.setValue(DiagonalBlockV2.NORTH_EAST, Boolean.FALSE).setValue(DiagonalBlockV2.SOUTH_EAST, Boolean.FALSE).setValue(DiagonalBlockV2.SOUTH_WEST, Boolean.FALSE).setValue(DiagonalBlockV2.NORTH_WEST, Boolean.FALSE);
    }

    default void _createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(DiagonalBlockV2.NORTH_EAST, DiagonalBlockV2.SOUTH_EAST, DiagonalBlockV2.SOUTH_WEST, DiagonalBlockV2.NORTH_WEST);
    }

    default BlockState _getStateForPlacement(BlockPlaceContext context, BlockState blockState) {
        Level level = context.getLevel();
        BlockPos clickedPos = context.getClickedPos();
        EightWayDirection[] eightWayDirections = EightWayDirection.getIntercardinalDirections();
        return this.updateIntercardinalDirections(blockState, level, clickedPos, eightWayDirections);
    }

    default BlockState updateIntercardinalDirections(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, EightWayDirection... eightWayDirections) {
        for (EightWayDirection eightWayDirection : eightWayDirections) {
            BlockPos neighborBlockPos = blockPos.offset(eightWayDirection.getX(), eightWayDirection.getY(), eightWayDirection.getZ());
            boolean value = this.attachesDiagonallyTo(levelAccessor.getBlockState(neighborBlockPos));
            value = value && this.isFreeForDiagonals(levelAccessor, blockPos, eightWayDirection);
            value = value && this.isFreeForDiagonals(levelAccessor, neighborBlockPos, eightWayDirection.getOpposite());
            blockState = blockState.setValue(PROPERTY_BY_DIRECTION.get(eightWayDirection), value);
        }
        return blockState;
    }

    private boolean isFreeForDiagonals(BlockGetter blockGetter, BlockPos blockPos, EightWayDirection eightWayDirection) {
        for (EightWayDirection neighbor : eightWayDirection.getCardinalNeighbors()) {
            Direction direction = neighbor.toDirection();
            BlockPos neighborBlockPos = blockPos.relative(direction);
            BlockState neighborBlockState = blockGetter.getBlockState(neighborBlockPos);
            if (this.attachesDirectlyTo(neighborBlockState, neighborBlockState.isFaceSturdy(blockGetter, neighborBlockPos, direction.getOpposite()), direction.getOpposite())) {
                return false;
            }
        }
        return true;
    }

    default BlockState _updateShape(BlockState blockState, Direction direction, BlockState neighboringBlockState, LevelAccessor levelAccessor, BlockPos blockPos, BlockPos neighboringBlockPos) {
        if (direction.getAxis().getPlane() == Direction.Plane.HORIZONTAL) {
            EightWayDirection[] eightWayDirections = EightWayDirection.toEightWayDirection(direction).getIntercardinalNeighbors();
            return this.updateIntercardinalDirections(blockState, levelAccessor, blockPos, eightWayDirections);
        }
        return blockState;
    }

    default void _updateIndirectNeighbourShapes(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, int flags, int recursionLeft) {
        for (EightWayDirection eightWayDirection : EightWayDirection.getIntercardinalDirections()) {
            if (blockState.getValue(PROPERTY_BY_DIRECTION.get(eightWayDirection))) {
                BlockPos neighborBlockPos = blockPos.offset(eightWayDirection.getX(), eightWayDirection.getY(), eightWayDirection.getZ());
                BlockState neighborBlockState = levelAccessor.getBlockState(neighborBlockPos);
                if (neighborBlockState.getBlock() instanceof DiagonalBlockV2) {
                    BlockState newNeighborBlockState = this.updateIntercardinalDirections(neighborBlockState, levelAccessor, neighborBlockPos, eightWayDirection.getOpposite());
                    Block.updateOrDestroy(neighborBlockState, newNeighborBlockState, levelAccessor, neighborBlockPos, flags, recursionLeft);
                }
            }
        }
    }
}
