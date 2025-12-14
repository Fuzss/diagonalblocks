package fuzs.diagonalblocks.api.v2.impl;

import fuzs.diagonalblocks.api.v2.DiagonalBlock;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.EightWayDirection;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.ScheduledTickAccess;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.Map;
import java.util.Stack;
import java.util.function.Function;
import java.util.stream.Collectors;

public class DiagonalWallBlock extends LegacyWallBlock implements StarCollisionBlock {
    protected static final Direction[] UPDATE_SHAPE_ORDER = new Direction[]{
            Direction.WEST, Direction.EAST, Direction.NORTH, Direction.SOUTH
    };

    public DiagonalWallBlock(BlockBehaviour.Properties properties) {
        super(properties);
        this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
    }

    @Override
    protected Function<BlockState, VoxelShape> makeShapes(float nodeWidth, float nodeHeight, float extensionWidth, float extensionBottom, float extensionHeight) {
        return this._makeShapes(nodeWidth, nodeHeight, extensionWidth, extensionBottom, extensionHeight);
    }

    @Override
    public void updateIndirectNeighbourShapes(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, int flags, int recursionLeft) {
        this._updateIndirectNeighbourShapes(blockState, levelAccessor, blockPos, flags, recursionLeft);
    }

    @Override
    public BlockState updateNonDiagonalIndirectNeighbourShapes(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, int flags, int recursionLeft) {
        blockState = StarCollisionBlock.super.updateNonDiagonalIndirectNeighbourShapes(blockState,
                levelAccessor,
                blockPos,
                flags,
                recursionLeft);
        if (!(blockState.getBlock() instanceof DiagonalWallBlock)) {
            for (Direction direction : UPDATE_SHAPE_ORDER) {
                BlockPos neighboringBlockPos = blockPos.relative(direction);
                blockState = blockState.getBlock()
                        .updateShape(blockState,
                                levelAccessor,
                                levelAccessor,
                                blockPos,
                                direction,
                                neighboringBlockPos,
                                levelAccessor.getBlockState(neighboringBlockPos),
                                levelAccessor.getRandom());
            }
            return blockState;
        } else {
            return blockState;
        }
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        this._createBlockStateDefinition(builder);
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        BlockState blockState = this._getStateForPlacement(context, super.getStateForPlacement(context));
        blockState = this.shouldNotRaisePost(context.getLevel(), context.getClickedPos(), blockState) ?
                blockState.trySetValue(LegacyWallBlock.UP, Boolean.FALSE) : blockState;
        if (!(blockState.getBlock() instanceof DiagonalWallBlock)) {
            return blockState.getBlock().getStateForPlacement(context);
        } else {
            return blockState;
        }
    }

    @Override
    public BlockState getNonDiagonalStateForPlacement(BlockPlaceContext context, BlockState blockState) {
        blockState = StarCollisionBlock.super.getNonDiagonalStateForPlacement(context, blockState);
        if (!(blockState.getBlock() instanceof DiagonalWallBlock)) {
            return blockState.getBlock().getStateForPlacement(context);
        } else {
            return blockState;
        }
    }

    @Override
    public BlockState updateShape(BlockState blockState, LevelReader levelReader, ScheduledTickAccess scheduledTickAccess, BlockPos blockPos, Direction direction, BlockPos neighboringBlockPos, BlockState neighboringBlockState, RandomSource randomSource) {
        blockState = super.updateShape(blockState,
                levelReader,
                scheduledTickAccess,
                blockPos,
                direction,
                neighboringBlockPos,
                neighboringBlockState,
                randomSource);
        blockState = this._updateShape(blockState,
                levelReader,
                scheduledTickAccess,
                blockPos,
                direction,
                neighboringBlockPos,
                neighboringBlockState,
                randomSource);
        blockState = direction != Direction.DOWN && this.shouldNotRaisePost(levelReader, blockPos, blockState) ?
                blockState.trySetValue(LegacyWallBlock.UP, Boolean.FALSE) : blockState;
        if (!(blockState.getBlock() instanceof DiagonalWallBlock)) {
            return blockState.getBlock()
                    .updateShape(blockState,
                            levelReader,
                            scheduledTickAccess,
                            blockPos,
                            direction,
                            neighboringBlockPos,
                            neighboringBlockState,
                            randomSource);
        } else {
            return blockState;
        }
    }

    @Override
    public BlockState updateNonDiagonalShape(BlockState blockState, LevelReader levelReader, ScheduledTickAccess scheduledTickAccess, BlockPos blockPos, Direction direction, BlockPos neighboringBlockPos, BlockState neighboringBlockState, RandomSource randomSource) {
        blockState = StarCollisionBlock.super.updateNonDiagonalShape(blockState,
                levelReader,
                scheduledTickAccess,
                blockPos,
                direction,
                neighboringBlockPos,
                neighboringBlockState,
                randomSource);
        if (!(blockState.getBlock() instanceof DiagonalWallBlock)) {
            return blockState.getBlock()
                    .updateShape(blockState,
                            levelReader,
                            scheduledTickAccess,
                            blockPos,
                            direction,
                            neighboringBlockPos,
                            neighboringBlockState,
                            randomSource);
        } else {
            return blockState;
        }
    }

    private boolean shouldNotRaisePost(LevelReader level, BlockPos blockPos, BlockState blockState) {
        // this method is designed for WallBlock::getStateForPlacement and WallBlock::updateShape
        // to run after all the other block state properties have been set
        // only diagonal properties must be checked as the calling method has already resolved cardinal directions
        if (blockState.getValueOrElse(LegacyWallBlock.UP, Boolean.FALSE)) {
            if (!this.shouldRaisePost(level, blockPos)) {
                boolean northEast = blockState.getValueOrElse(NORTH_EAST, Boolean.FALSE);
                boolean southEast = blockState.getValueOrElse(SOUTH_EAST, Boolean.FALSE);
                boolean southWest = blockState.getValueOrElse(SOUTH_WEST, Boolean.FALSE);
                boolean northWest = blockState.getValueOrElse(NORTH_WEST, Boolean.FALSE);
                return northEast && southWest && !southEast && !northWest
                        || !northEast && !southWest && southEast && northWest;
            }
        }
        return false;
    }

    @Override
    public DiagonalBlockType getType() {
        return DiagonalBlockTypes.WALL;
    }

    @Override
    public boolean attachesDirectlyTo(BlockState blockState, boolean isSideSolid, Direction direction) {
        return this.connectsTo(blockState, isSideSolid, direction);
    }

    @Override
    public boolean attachesDiagonallyTo(BlockState blockState, EightWayDirection eightWayDirection) {
        return StarCollisionBlock.super.attachesDiagonallyTo(blockState, eightWayDirection)
                || blockState.getBlock() instanceof DiagonalBlock diagonalBlock
                && diagonalBlock.getType() == DiagonalBlockTypes.WINDOW;
    }

    @Override
    public BlockState updateIndirectNeighborDiagonalProperty(BlockState neighborBlockState, LevelAccessor levelAccessor, BlockPos blockPos, EightWayDirection eightWayDirection) {
        BlockState blockState = StarCollisionBlock.super.updateIndirectNeighborDiagonalProperty(neighborBlockState,
                levelAccessor,
                blockPos,
                eightWayDirection);
        return blockState.setValue(LegacyWallBlock.UP, this.shouldRaisePost(levelAccessor, blockPos, blockState));
    }

    private boolean shouldRaisePost(LevelReader level, BlockPos blockPos, BlockState blockState) {
        // does a complete check for raising the post depending on all eight direction properties,
        // as this method is designed for updates from indirect neighbors which do not result in the post having previously been updated
        // based on cardinal directions as is the case otherwise for WallBlock::getStateForPlacement and WallBlock::updateShape
        Stack<EightWayDirection> stack = StarCollisionBlock.PROPERTY_BY_DIRECTION.entrySet()
                .stream()
                .filter(entry -> blockState.getValue(entry.getValue()))
                .map(Map.Entry::getKey)
                .collect(Collectors.toCollection(Stack::new));
        if (!stack.isEmpty()) {
            while (stack.size() >= 2) {
                if (stack.pop().getOpposite() != stack.pop()) {
                    return true;
                }
            }
            return !stack.isEmpty() || this.shouldRaisePost(level, blockPos);
        } else {
            return true;
        }
    }
}
