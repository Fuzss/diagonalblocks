package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.api.v2.DiagonalBlockV2;
import fuzs.diagonalfences.api.world.level.block.EightWayDirection;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.VoxelShape;

public class DiagonalWallBlock extends LegacyWallBlock implements StarCollisionBlockAdapter {
    private Object2IntMap<BlockState> statePaletteMap;
    private final Block block;

    public DiagonalWallBlock(Block block) {
        super(BlockBehaviour.Properties.copy(block));
        this.block = block;
        this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
    }

    @Override
    public String getDescriptionId() {
        return this.block.getDescriptionId();
    }

    @Override
    protected VoxelShape[] makeShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {
        return this.getShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
    }

    @Override
    protected int getAABBIndex(BlockState state) {
        if (this.statePaletteMap == null) this.statePaletteMap = new Object2IntOpenHashMap<>();
        return this.statePaletteMap.computeIfAbsent(state, this::makeIndex);
    }

    @Override
    public void updateIndirectNeighbourShapes(BlockState state, LevelAccessor world, BlockPos pos, int flags, int recursionLeft) {
        this._updateIndirectNeighbourShapes(state, world, pos, flags, recursionLeft);
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
        BlockState newBlockState = super.updateShape(blockState, direction, neighboringBlockState, levelAccessor, blockPos, neighboringBlockPos);
        return this._updateShape(blockState, direction, neighboringBlockState, levelAccessor, blockPos, neighboringBlockPos, newBlockState);
    }

    @Override
    public boolean canConnectToMe(BlockState neighborState, EightWayDirection neighborDirectionToMe) {
        if (neighborState.getBlock() instanceof LegacyWallBlock && ((StarCollisionBlock) neighborState.getBlock()).supportsDiagonalConnections()) {
            for (EightWayDirection neighbor : neighborDirectionToMe.getCardinalNeighbors()) {
                if (neighborState.getValue(DIRECTION_TO_PROPERTY_MAP.get(neighbor))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    @Override
    public DiagonalBlockType getType() {
        return DiagonalBlockType.WALLS;
    }

    @Override
    public boolean attachesTo(BlockState blockState, BlockState neighboringBlockState) {
        return StarCollisionBlockAdapter.super.attachesTo(blockState, neighboringBlockState) || neighboringBlockState.getBlock() instanceof DiagonalBlockV2 diagonalBlock && diagonalBlock.getType() == DiagonalBlockType.WINDOWS;
    }
}
