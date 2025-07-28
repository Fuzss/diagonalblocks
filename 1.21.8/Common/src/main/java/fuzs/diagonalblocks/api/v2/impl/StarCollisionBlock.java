package fuzs.diagonalblocks.api.v2.impl;

import com.google.common.collect.Maps;
import fuzs.diagonalblocks.api.v2.DiagonalBlock;
import fuzs.diagonalblocks.api.v2.EightWayDirection;
import fuzs.diagonalblocks.init.ModRegistry;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.ScheduledTickAccess;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.phys.shapes.BooleanOp;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

public interface StarCollisionBlock extends DiagonalBlock, StarShapeProvider {
    Map<EightWayDirection, BooleanProperty> PROPERTY_BY_DIRECTION = Maps.immutableEnumMap(Map.of(EightWayDirection.NORTH,
            DiagonalBlock.NORTH,
            EightWayDirection.EAST,
            DiagonalBlock.EAST,
            EightWayDirection.SOUTH,
            DiagonalBlock.SOUTH,
            EightWayDirection.WEST,
            DiagonalBlock.WEST,
            EightWayDirection.NORTH_EAST,
            DiagonalBlock.NORTH_EAST,
            EightWayDirection.SOUTH_EAST,
            DiagonalBlock.SOUTH_EAST,
            EightWayDirection.SOUTH_WEST,
            DiagonalBlock.SOUTH_WEST,
            EightWayDirection.NORTH_WEST,
            DiagonalBlock.NORTH_WEST));
    Int2ObjectMap<Map<EightWayDirection, VoxelShape>> CORNER_SHAPES_CACHE = new Int2ObjectOpenHashMap<>();
    Object2ObjectMap<DiagonalBlock, Map<EightWayDirection, VoxelShape>> CORNER_SHAPES_BLOCK_CACHE = new Object2ObjectOpenHashMap<>();

    static BlockState updateDiagonalProperties(DiagonalBlock diagonalBlock, BlockState blockState, LevelReader levelReader, BlockPos blockPos, EightWayDirection... eightWayDirections) {
        for (EightWayDirection eightWayDirection : eightWayDirections) {
            if (!eightWayDirection.isIntercardinal()) {
                throw new IllegalArgumentException("direction must be intercardinal");
            }
            BlockPos neighborBlockPos = blockPos.offset(eightWayDirection.getX(),
                    eightWayDirection.getY(),
                    eightWayDirection.getZ());
            BlockState originalNeighborBlockState = levelReader.getBlockState(neighborBlockPos);
            BlockState neighborBlockState = DiagonalBlockTypeImpl.NON_DIAGONAL_TO_DIAGONAL_BLOCK_STATES.getOrDefault(
                    originalNeighborBlockState,
                    originalNeighborBlockState);
            boolean allowsDiagonalProperty = allowsDiagonalProperty(diagonalBlock,
                    levelReader,
                    blockPos,
                    neighborBlockState,
                    eightWayDirection);
            if (allowsDiagonalProperty) {
                DiagonalBlock neighborDiagonalBlock = (DiagonalBlock) neighborBlockState.getBlock();
                allowsDiagonalProperty = allowsDiagonalProperty(neighborDiagonalBlock,
                        levelReader,
                        neighborBlockPos,
                        blockState,
                        eightWayDirection.getOpposite());
            }
            blockState = blockState.setValue(PROPERTY_BY_DIRECTION.get(eightWayDirection), allowsDiagonalProperty);
        }
        return blockState;
    }

    static boolean allowsDiagonalProperty(DiagonalBlock diagonalBlock, LevelReader levelReader, BlockPos blockPos, BlockState neighborBlockState, EightWayDirection eightWayDirection) {
        return diagonalBlock.attachesDiagonallyTo(neighborBlockState, eightWayDirection.getOpposite())
                && isFreeForDiagonalProperty(diagonalBlock, levelReader, blockPos, eightWayDirection)
                && isNotCollidingWithNeighbors(diagonalBlock, levelReader, blockPos, eightWayDirection);
    }

    static boolean isFreeForDiagonalProperty(DiagonalBlock diagonalBlock, LevelReader levelReader, BlockPos blockPos, EightWayDirection eightWayDirection) {
        for (EightWayDirection neighbor : eightWayDirection.getCardinalNeighbors()) {
            Direction direction = neighbor.toDirection();
            BlockPos neighborBlockPos = blockPos.relative(direction);
            BlockState neighborBlockState = levelReader.getBlockState(neighborBlockPos);
            boolean isSideSolid = neighborBlockState.isFaceSturdy(levelReader,
                    neighborBlockPos,
                    direction.getOpposite());
            // do not use block state values, they might not have been updated yet on the other block
            if (diagonalBlock.attachesDirectlyTo(neighborBlockState, isSideSolid, direction.getOpposite())) {
                return false;
            }
        }
        return true;
    }

    static boolean isNotCollidingWithNeighbors(DiagonalBlock diagonalBlock, LevelReader levelReader, BlockPos blockPos, EightWayDirection eightWayDirection) {
        if (CORNER_SHAPES_BLOCK_CACHE.containsKey(diagonalBlock)) {
            Map<EightWayDirection, VoxelShape> shapes = CORNER_SHAPES_BLOCK_CACHE.get(diagonalBlock);
            for (EightWayDirection neighbor : eightWayDirection.getCardinalNeighbors()) {
                VoxelShape cornerShape = shapes.get(
                        eightWayDirection.data2d != neighbor.data2d ? eightWayDirection.rotateClockWise(2) :
                                eightWayDirection.rotateCounterClockWise(2));
                BlockPos neighborBlockPos = blockPos.relative(neighbor.toDirection());
                BlockState neighborBlockState = levelReader.getBlockState(neighborBlockPos);
                if (!neighborBlockState.is(ModRegistry.NEVER_BLOCKS_DIAGONAL_CONNECTIONS_BLOCK_TAG)) {
                    VoxelShape voxelShape = neighborBlockState.getCollisionShape(levelReader, neighborBlockPos);
                    if (Shapes.joinIsNotEmpty(cornerShape, voxelShape, BooleanOp.AND)) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    @Override
    default VoxelShape[] _makeLegacyShapes(float nodeWidth, float extensionWidth, float nodeTop, float extensionBottom, float extensionTop) {
        if (extensionBottom == 0.0F && extensionTop <= 16.0F && !CORNER_SHAPES_BLOCK_CACHE.containsKey(this)) {
            Map<EightWayDirection, VoxelShape> cornerShapes = CORNER_SHAPES_CACHE.computeIfAbsent(Arrays.hashCode(new float[]{
                    extensionWidth, extensionBottom, extensionTop
            }), (int hashCode) -> {
                return (Map<EightWayDirection, VoxelShape>) Stream.of(EightWayDirection.getIntercardinalDirections())
                        .collect(Maps.<EightWayDirection, EightWayDirection, VoxelShape>toImmutableEnumMap(Function.identity(),
                                (EightWayDirection eightWayDirection) -> {
                                    return this.getCornerShape(eightWayDirection,
                                            extensionWidth,
                                            extensionBottom,
                                            extensionTop);
                                }));
            });
            CORNER_SHAPES_BLOCK_CACHE.put(this, cornerShapes);
        }
        return StarShapeProvider.super._makeLegacyShapes(nodeWidth,
                extensionWidth,
                nodeTop,
                extensionBottom,
                extensionTop);
    }

    private VoxelShape getCornerShape(EightWayDirection eightWayDirection, float extensionWidth, float extensionBottom, float extensionTop) {
        int posX = eightWayDirection.getX() > 0 ? 0 : 16;
        int posZ = eightWayDirection.getZ() > 0 ? 0 : 16;
        return Block.box(posX - extensionWidth,
                extensionBottom,
                posZ - extensionWidth,
                posX + extensionWidth,
                extensionTop,
                posZ + extensionWidth);
    }

    @Override
    default boolean attachesDiagonallyTo(BlockState blockState, EightWayDirection eightWayDirection) {
        return blockState.getBlock() instanceof DiagonalBlock diagonalBlock
                && diagonalBlock.getType() == this.getType();
    }

    default BlockState addDefaultStates(BlockState defaultState) {
        return defaultState.setValue(DiagonalBlock.NORTH_EAST, Boolean.FALSE)
                .setValue(DiagonalBlock.SOUTH_EAST, Boolean.FALSE)
                .setValue(DiagonalBlock.SOUTH_WEST, Boolean.FALSE)
                .setValue(DiagonalBlock.NORTH_WEST, Boolean.FALSE);
    }

    /**
     * See {@link Block#createBlockStateDefinition(StateDefinition.Builder)}.
     */
    default void _createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(DiagonalBlock.NORTH_EAST,
                DiagonalBlock.SOUTH_EAST,
                DiagonalBlock.SOUTH_WEST,
                DiagonalBlock.NORTH_WEST);
    }

    /**
     * See {@link Block#getStateForPlacement(BlockPlaceContext)}.
     */
    default BlockState _getStateForPlacement(BlockPlaceContext context, BlockState blockState) {
        Level level = context.getLevel();
        BlockPos clickedPos = context.getClickedPos();
        EightWayDirection[] eightWayDirections = EightWayDirection.getIntercardinalDirections();
        BlockState newBlockState = updateDiagonalProperties(this, blockState, level, clickedPos, eightWayDirections);
        return this.getNonDiagonalStateForPlacement(context, newBlockState);
    }

    default BlockState getNonDiagonalStateForPlacement(BlockPlaceContext context, BlockState blockState) {
        return this.getNonDiagonalBlockIfPossible(blockState);
    }

    /**
     * See
     * {@link Block#updateShape(BlockState, LevelReader, ScheduledTickAccess, BlockPos, Direction, BlockPos, BlockState,
     * RandomSource)}.
     */
    default BlockState _updateShape(BlockState blockState, LevelReader levelReader, ScheduledTickAccess scheduledTickAccess, BlockPos blockPos, Direction direction, BlockPos neighboringBlockPos, BlockState neighboringBlockState, RandomSource randomSource) {
        BlockState newBlockState;
        if (direction.getAxis().getPlane() == Direction.Plane.HORIZONTAL) {
            EightWayDirection[] eightWayDirections = EightWayDirection.toEightWayDirection(direction)
                    .getIntercardinalNeighbors();
            newBlockState = updateDiagonalProperties(this, blockState, levelReader, blockPos, eightWayDirections);
        } else {
            newBlockState = blockState;
        }

        return this.updateNonDiagonalShape(newBlockState,
                levelReader,
                scheduledTickAccess,
                blockPos,
                direction,
                neighboringBlockPos,
                neighboringBlockState,
                randomSource);
    }

    default BlockState updateNonDiagonalShape(BlockState blockState, LevelReader levelReader, ScheduledTickAccess scheduledTickAccess, BlockPos blockPos, Direction direction, BlockPos neighboringBlockPos, BlockState neighboringBlockState, RandomSource randomSource) {
        return this.getNonDiagonalBlockIfPossible(blockState);
    }

    /**
     * See {@link Block#updateIndirectNeighbourShapes(BlockState, LevelAccessor, BlockPos, int, int)}.
     */
    default void _updateIndirectNeighbourShapes(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, int flags, int recursionLeft) {
        for (EightWayDirection eightWayDirection : EightWayDirection.getIntercardinalDirections()) {
            if (blockState.getValue(PROPERTY_BY_DIRECTION.get(eightWayDirection))) {
                BlockPos neighborBlockPos = blockPos.offset(eightWayDirection.getX(),
                        eightWayDirection.getY(),
                        eightWayDirection.getZ());
                BlockState originalNeighborBlockState = levelAccessor.getBlockState(neighborBlockPos);
                BlockState neighborBlockState = DiagonalBlockTypeImpl.NON_DIAGONAL_TO_DIAGONAL_BLOCK_STATES.getOrDefault(
                        originalNeighborBlockState,
                        originalNeighborBlockState);
                BlockState newNeighborBlockState;
                if (neighborBlockState.getBlock() instanceof StarCollisionBlock starCollisionBlock) {
                    newNeighborBlockState = starCollisionBlock.updateIndirectNeighborDiagonalProperty(neighborBlockState,
                            levelAccessor,
                            neighborBlockPos,
                            eightWayDirection.getOpposite());
                } else if (neighborBlockState.getBlock() instanceof DiagonalBlock diagonalBlock) {
                    newNeighborBlockState = updateDiagonalProperties(diagonalBlock,
                            neighborBlockState,
                            levelAccessor,
                            neighborBlockPos,
                            eightWayDirection.getOpposite());
                } else {
                    newNeighborBlockState = null;
                }
                if (newNeighborBlockState != null) {
                    Block.updateOrDestroy(originalNeighborBlockState,
                            this.updateNonDiagonalIndirectNeighbourShapes(newNeighborBlockState,
                                    levelAccessor,
                                    blockPos,
                                    flags,
                                    recursionLeft),
                            levelAccessor,
                            neighborBlockPos,
                            flags,
                            recursionLeft);
                }
            }
        }
    }

    default BlockState updateNonDiagonalIndirectNeighbourShapes(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, int flags, int recursionLeft) {
        return this.getNonDiagonalBlockIfPossible(blockState);
    }

    private BlockState getNonDiagonalBlockIfPossible(BlockState blockState) {
        return DiagonalBlockTypeImpl.DIAGONAL_TO_NON_DIAGONAL_BLOCK_STATES.getOrDefault(blockState, blockState);
    }

    default BlockState updateIndirectNeighborDiagonalProperty(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, EightWayDirection eightWayDirection) {
        return updateDiagonalProperties(this, blockState, levelAccessor, blockPos, eightWayDirection);
    }
}
