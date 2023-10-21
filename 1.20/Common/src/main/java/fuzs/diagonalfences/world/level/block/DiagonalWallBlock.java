package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.api.world.level.block.EightWayDirection;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.WallBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.phys.shapes.VoxelShape;

public class DiagonalWallBlock extends LegacyWallBlock implements StarCollisionBlockAdapter {
    private Object2IntMap<BlockState> statePaletteMap;
    private final WallBlock wallBlock;

    public DiagonalWallBlock(WallBlock wallBlock, Properties properties) {
        super(properties);
        this.wallBlock = wallBlock;
        this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
    }

    @Override
    public String getDescriptionId() {
        return this.wallBlock.getDescriptionId();
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
        this.createBlockStateDefinition2(builder);
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        BlockGetter level = context.getLevel();
        BlockPos pos = context.getClickedPos();
        FluidState fluidState = context.getLevel().getFluidState(context.getClickedPos());
        BlockState placementState = super.getStateForPlacement(context);
        return this.makeStateForPlacement(placementState, level, pos, fluidState);
    }

    @Override
    public BlockState updateShape(BlockState blockState, Direction direction, BlockState blockState2, LevelAccessor levelAccessor, BlockPos blockPos, BlockPos blockPos2) {
        BlockState updatedShape = super.updateShape(blockState, direction, blockState2, levelAccessor, blockPos, blockPos2);
        BlockState newBlockState = this.updateShape2(blockState, direction, blockState2, levelAccessor, blockPos, blockPos2, updatedShape);
        return newBlockState != null ? newBlockState : updatedShape;
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
    public boolean canConnect(BlockGetter blockGetter, BlockPos position, BlockState state, Direction direction) {
        return this.connectsTo(state, state.isFaceSturdy(blockGetter, position, direction), direction);
    }

}
