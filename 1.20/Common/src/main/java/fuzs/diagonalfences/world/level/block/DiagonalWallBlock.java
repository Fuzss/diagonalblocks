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
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.Map;
import java.util.Stack;
import java.util.stream.Collectors;

public class DiagonalWallBlock extends LegacyWallBlock implements StarCollisionBlock {
    private Object2IntMap<BlockState> statePaletteMap;
    private final Block block;

    public DiagonalWallBlock(Block block) {
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
        return this.getShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
    }

    @Override
    protected int getAABBIndex(BlockState state) {
        if (this.statePaletteMap == null) this.statePaletteMap = new Object2IntOpenHashMap<>();
        return this.statePaletteMap.computeIfAbsent(state, this::makeIndex);
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
        BlockState blockState = this._getStateForPlacement(context, super.getStateForPlacement(context));
        return this.shouldNotRaisePost(context.getLevel(), context.getClickedPos(), blockState) ? blockState.setValue(LegacyWallBlock.UP, false) : blockState;
    }

    @Override
    public BlockState updateShape(BlockState blockState, Direction direction, BlockState neighboringBlockState, LevelAccessor levelAccessor, BlockPos blockPos, BlockPos neighboringBlockPos) {
        blockState = super.updateShape(blockState, direction, neighboringBlockState, levelAccessor, blockPos, neighboringBlockPos);
        blockState = this._updateShape(blockState, direction, neighboringBlockState, levelAccessor, blockPos, neighboringBlockPos);
        return direction != Direction.DOWN && this.shouldNotRaisePost(levelAccessor, blockPos, blockState) ? blockState.setValue(LegacyWallBlock.UP, false) : blockState;
    }

    private boolean shouldNotRaisePost(LevelReader level, BlockPos blockPos, BlockState blockState) {
        // this method is designed for WallBlock::getStateForPlacement and WallBlock::updateShape to run after all other block state properties have been set
        // only diagonal properties must be checked as the calling method has already resolved cardinal directions
        if (blockState.getValue(LegacyWallBlock.UP)) {
            if (!this.shouldRaisePost(level, blockPos)) {
                boolean northEast = blockState.getValue(NORTH_EAST);
                boolean southEast = blockState.getValue(SOUTH_EAST);
                boolean southWest = blockState.getValue(SOUTH_WEST);
                boolean northWest = blockState.getValue(NORTH_WEST);
                return northEast && southWest && !southEast && !northWest || !northEast && !southWest && southEast && northWest;
            }
        }
        return false;
    }

    @Override
    public DiagonalBlockType getType() {
        return DiagonalBlockType.WALLS;
    }

    @Override
    public boolean attachesDirectlyTo(BlockState blockState, boolean isSideSolid, Direction direction) {
        return this.connectsTo(blockState, isSideSolid, direction);
    }

    @Override
    public boolean attachesDiagonallyTo(BlockState blockState) {
        return StarCollisionBlock.super.attachesDiagonallyTo(blockState) || blockState.getBlock() instanceof DiagonalBlockV2 diagonalBlock && diagonalBlock.getType() == DiagonalBlockType.WINDOWS;
    }

    @Override
    public BlockState updateIndirectNeighborDiagonalProperty(BlockState neighborBlockState, LevelAccessor levelAccessor, BlockPos blockPos, EightWayDirection eightWayDirection) {
        BlockState blockState = StarCollisionBlock.super.updateIndirectNeighborDiagonalProperty(neighborBlockState, levelAccessor, blockPos, eightWayDirection);
        return blockState.setValue(LegacyWallBlock.UP, this.shouldRaisePost(levelAccessor, blockPos, blockState));
    }

    private boolean shouldRaisePost(LevelReader level, BlockPos blockPos, BlockState blockState) {
        // does a complete check for raising the post depending on all eight direction properties,
        // as this method is designed for updates from indirect neighbors which do not result in the post having previously been updated
        // based on cardinal directions as is the case otherwise for WallBlock::getStateForPlacement and WallBlock::updateShape
        Stack<EightWayDirection> stack = StarCollisionBlock.PROPERTY_BY_DIRECTION.entrySet().stream()
                .filter(entry -> blockState.getValue(entry.getValue()))
                .map(Map.Entry::getKey)
                .collect(Collectors.toCollection(Stack::new));
        while (stack.size() >= 2) {
            if (stack.pop().getOpposite() != stack.pop()) {
                return true;
            }
        }
        return !stack.isEmpty() || this.shouldRaisePost(level, blockPos);
    }
}
