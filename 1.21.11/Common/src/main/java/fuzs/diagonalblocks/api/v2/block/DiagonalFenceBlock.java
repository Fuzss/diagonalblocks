package fuzs.diagonalblocks.api.v2.block;

import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.util.EightWayDirection;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.tags.BlockTags;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.ScheduledTickAccess;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.function.Function;

public class DiagonalFenceBlock extends FenceBlock implements StarCollisionBlock {

    public DiagonalFenceBlock(BlockBehaviour.Properties properties) {
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
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        this._createBlockStateDefinition(builder);
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return this._getStateForPlacement(context, super.getStateForPlacement(context));
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
        return this._updateShape(blockState,
                levelReader,
                scheduledTickAccess,
                blockPos,
                direction,
                neighboringBlockPos,
                neighboringBlockState,
                randomSource);
    }

    @Override
    public DiagonalBlockType getType() {
        return DiagonalBlockTypes.FENCE;
    }

    @Override
    public boolean attachesDirectlyTo(BlockState blockState, boolean isSideSolid, Direction direction) {
        return this.connectsTo(blockState, isSideSolid, direction);
    }

    @Override
    public boolean attachesDiagonallyTo(BlockState blockState, EightWayDirection eightWayDirection) {
        return StarCollisionBlock.super.attachesDiagonallyTo(blockState, eightWayDirection)
                && blockState.is(BlockTags.FENCES) && blockState.is(BlockTags.WOODEN_FENCES) == this.defaultBlockState()
                .is(BlockTags.WOODEN_FENCES);
    }
}
