package fuzs.diagonalblocks.api.v2.impl;

import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.tags.BlockTags;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.VoxelShape;

public class DiagonalFenceBlock extends FenceBlock implements StarCollisionBlock {
    private final Block block;

    public DiagonalFenceBlock(Block block) {
        super(BlockBehaviour.Properties.copy(block).dropsLike(block));
        this.block = block;
        this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
    }

    @Override
    public String getDescriptionId() {
        return this.block.getDescriptionId();
    }

    @Override
    protected VoxelShape[] makeShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {
        return this._makeShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
    }

    @Override
    protected int getAABBIndex(BlockState blockState) {
        return this._getAABBIndex(blockState);
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
    public BlockState updateShape(BlockState blockState, Direction direction, BlockState neighboringBlockState, LevelAccessor levelAccessor, BlockPos blockPos, BlockPos neighboringBlockPos) {
        blockState = super.updateShape(blockState, direction, neighboringBlockState, levelAccessor, blockPos, neighboringBlockPos);
        return this._updateShape(blockState, direction, neighboringBlockState, levelAccessor, blockPos, neighboringBlockPos);
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
    public boolean attachesDiagonallyTo(BlockState blockState) {
        return StarCollisionBlock.super.attachesDiagonallyTo(blockState) && blockState.is(BlockTags.FENCES) && blockState.is(BlockTags.WOODEN_FENCES) == this.defaultBlockState().is(BlockTags.WOODEN_FENCES);
    }
}
