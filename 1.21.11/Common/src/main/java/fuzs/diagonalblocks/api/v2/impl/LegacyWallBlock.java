package fuzs.diagonalblocks.api.v2.impl;

import com.google.common.collect.ImmutableMap;
import com.mojang.serialization.MapCodec;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.tags.BlockTags;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.ScheduledTickAccess;
import net.minecraft.world.level.block.*;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.level.pathfinder.PathComputationType;
import net.minecraft.world.phys.shapes.BooleanOp;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.Map;
import java.util.function.Function;

/**
 * The wall block implementation from Minecraft 1.15.2 before walls received additional block states by splitting the
 * directional boolean properties into enum properties with three possible values (see
 * {@link net.minecraft.world.level.block.state.properties.WallSide}).
 *
 * <p>Since this mod already adds a good chunk of new properties, combined with the number of properties
 * present in vanilla since Minecraft 1.16, there would simply be too many block states for the game to sufficiently
 * handle (3^4*2^2=324 possible states in vanilla, 3^8*2^2=26,244 possible states with diagonal block state
 * properties).
 *
 * <p>By simplifying the four directional states to boolean properties once again, the addition of diagonal connections
 * becomes manageable once more (2^4*2^2=64 possible states in vanilla, 2^8*2^2=1,024 possible states with diagonal
 * block state properties).
 */
public class LegacyWallBlock extends CrossCollisionBlock {
    public static final Map<Property<?>, Property<?>> WALL_SIDE_PROPERTIES = ImmutableMap.of(WallBlock.EAST,
            CrossCollisionBlock.EAST,
            WallBlock.NORTH,
            CrossCollisionBlock.NORTH,
            WallBlock.SOUTH,
            CrossCollisionBlock.SOUTH,
            WallBlock.WEST,
            CrossCollisionBlock.WEST);
    public static final MapCodec<LegacyWallBlock> CODEC = simpleCodec(LegacyWallBlock::new);
    private static final VoxelShape POST_TEST = Block.box(7.0, 0.0, 7.0, 9.0, 16.0, 9.0);
    public static final BooleanProperty UP = BlockStateProperties.UP;
    private final Function<BlockState, VoxelShape> shapeWithPostByIndex;
    private final Function<BlockState, VoxelShape> collisionShapeWithPostByIndex;
    private final Object2IntMap<BlockState> stateToIndex = new Object2IntOpenHashMap<>();

    public LegacyWallBlock(Block.Properties properties) {
        super(0.0F * 2.0F, 0.0F, 3.0F * 2.0F, 14.0F, 24.0F, properties);
        this.registerDefaultState(this.stateDefinition.any()
                .setValue(UP, Boolean.TRUE)
                .setValue(NORTH, Boolean.FALSE)
                .setValue(EAST, Boolean.FALSE)
                .setValue(SOUTH, Boolean.FALSE)
                .setValue(WEST, Boolean.FALSE)
                .setValue(WATERLOGGED, Boolean.FALSE));
        this.shapeWithPostByIndex = this.makeShapes(4.0F * 2.0F, 16.0F, 3.0F * 2.0F, 0.0F, 14.0F);
        this.collisionShapeWithPostByIndex = this.makeShapes(4.0F * 2.0F, 24.0F, 3.0F * 2.0F, 0.0F, 24.0F);
    }

    @Override
    protected MapCodec<? extends CrossCollisionBlock> codec() {
        return CODEC;
    }

    @Override
    public VoxelShape getShape(BlockState blockState, BlockGetter blockGetter, BlockPos blockPos, CollisionContext collisionContext) {
        return blockState.getValue(UP) ? this.shapeWithPostByIndex.apply(blockState) :
                super.getShape(blockState, blockGetter, blockPos, collisionContext);
    }

    @Override
    public VoxelShape getCollisionShape(BlockState blockState, BlockGetter blockGetter, BlockPos blockPos, CollisionContext collisionContext) {
        return blockState.getValue(UP) ? this.collisionShapeWithPostByIndex.apply(blockState) :
                super.getCollisionShape(blockState, blockGetter, blockPos, collisionContext);
    }

    @Override
    public boolean isPathfindable(BlockState blockState, PathComputationType pathComputationType) {
        return false;
    }

    public boolean connectsTo(BlockState state, boolean sideSolid, Direction direction) {
        Block block = state.getBlock();
        boolean bl = block instanceof FenceGateBlock && FenceGateBlock.connectsToDirection(state, direction);
        return state.is(BlockTags.WALLS) || !isExceptionForConnection(state) && sideSolid
                || block instanceof IronBarsBlock || bl;
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext blockPlaceContext) {
        LevelReader levelReader = blockPlaceContext.getLevel();
        BlockPos clickedPos = blockPlaceContext.getClickedPos();
        FluidState fluidState = blockPlaceContext.getLevel().getFluidState(blockPlaceContext.getClickedPos());
        BlockPos blockPosNorth = clickedPos.north();
        BlockPos blockPosEast = clickedPos.east();
        BlockPos blockPosSouth = clickedPos.south();
        BlockPos blockPosWest = clickedPos.west();
        BlockState blockStateNorth = levelReader.getBlockState(blockPosNorth);
        BlockState blockStateEast = levelReader.getBlockState(blockPosEast);
        BlockState blockStateSouth = levelReader.getBlockState(blockPosSouth);
        BlockState blockStateWest = levelReader.getBlockState(blockPosWest);
        boolean connectsToNorth = this.connectsTo(blockStateNorth,
                blockStateNorth.isFaceSturdy(levelReader, blockPosNorth, Direction.SOUTH),
                Direction.SOUTH);
        boolean connectsToEast = this.connectsTo(blockStateEast,
                blockStateEast.isFaceSturdy(levelReader, blockPosEast, Direction.WEST),
                Direction.WEST);
        boolean connectsToSouth = this.connectsTo(blockStateSouth,
                blockStateSouth.isFaceSturdy(levelReader, blockPosSouth, Direction.NORTH),
                Direction.NORTH);
        boolean connectsToWest = this.connectsTo(blockStateWest,
                blockStateWest.isFaceSturdy(levelReader, blockPosWest, Direction.EAST),
                Direction.EAST);
        boolean bl5 = (!connectsToNorth || connectsToEast || !connectsToSouth || connectsToWest) && (connectsToNorth
                || !connectsToEast || connectsToSouth || !connectsToWest);
        return this.defaultBlockState()
                .setValue(UP, bl5 || this.shouldRaisePost(levelReader, clickedPos))
                .setValue(NORTH, connectsToNorth)
                .setValue(EAST, connectsToEast)
                .setValue(SOUTH, connectsToSouth)
                .setValue(WEST, connectsToWest)
                .setValue(WATERLOGGED, fluidState.getType() == Fluids.WATER);
    }

    @Override
    public BlockState updateShape(BlockState blockState, LevelReader levelReader, ScheduledTickAccess scheduledTickAccess, BlockPos blockPos, Direction direction, BlockPos neighboringBlockPos, BlockState neighboringBlockState, RandomSource randomSource) {
        if (blockState.getValue(WATERLOGGED)) {
            scheduledTickAccess.scheduleTick(blockPos, Fluids.WATER, Fluids.WATER.getTickDelay(levelReader));
        }
        if (direction == Direction.DOWN) {
            return super.updateShape(blockState,
                    levelReader,
                    scheduledTickAccess,
                    blockPos,
                    direction,
                    neighboringBlockPos,
                    neighboringBlockState,
                    randomSource);
        } else {
            Direction direction2 = direction.getOpposite();
            boolean bl = direction == Direction.NORTH ? this.connectsTo(neighboringBlockState,
                    neighboringBlockState.isFaceSturdy(levelReader, neighboringBlockPos, direction2),
                    direction2) : blockState.getValue(NORTH);
            boolean bl2 = direction == Direction.EAST ? this.connectsTo(neighboringBlockState,
                    neighboringBlockState.isFaceSturdy(levelReader, neighboringBlockPos, direction2),
                    direction2) : blockState.getValue(EAST);
            boolean bl3 = direction == Direction.SOUTH ? this.connectsTo(neighboringBlockState,
                    neighboringBlockState.isFaceSturdy(levelReader, neighboringBlockPos, direction2),
                    direction2) : blockState.getValue(SOUTH);
            boolean bl4 = direction == Direction.WEST ? this.connectsTo(neighboringBlockState,
                    neighboringBlockState.isFaceSturdy(levelReader, neighboringBlockPos, direction2),
                    direction2) : blockState.getValue(WEST);
            boolean bl5 = (!bl || bl2 || !bl3 || bl4) && (bl || !bl2 || bl3 || !bl4);
            return blockState.setValue(UP, bl5 || this.shouldRaisePost(levelReader, blockPos))
                    .setValue(NORTH, bl)
                    .setValue(EAST, bl2)
                    .setValue(SOUTH, bl3)
                    .setValue(WEST, bl4);
        }
    }

    public boolean shouldRaisePost(LevelReader levelReader, BlockPos blockPos) {
        blockPos = blockPos.above();
        BlockState blockState = levelReader.getBlockState(blockPos);
        if (blockState.is(BlockTags.WALL_POST_OVERRIDE)) {
            return true;
        }
        VoxelShape voxelShape = blockState.getCollisionShape(levelReader, blockPos).getFaceShape(Direction.DOWN);
        return !Shapes.joinIsNotEmpty(POST_TEST, voxelShape, BooleanOp.ONLY_FIRST);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(UP, NORTH, EAST, WEST, SOUTH, WATERLOGGED);
    }
}

