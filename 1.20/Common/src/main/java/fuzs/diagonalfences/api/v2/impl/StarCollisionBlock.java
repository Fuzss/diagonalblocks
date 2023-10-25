package fuzs.diagonalfences.api.v2.impl;

import com.google.common.collect.Maps;
import fuzs.diagonalfences.api.v2.DiagonalBlockV2;
import fuzs.diagonalfences.api.v2.EightWayDirection;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
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

public interface StarCollisionBlock extends DiagonalBlockV2, StarShapeProvider {
    Map<EightWayDirection, BooleanProperty> PROPERTY_BY_DIRECTION = Maps.immutableEnumMap(Map.of(EightWayDirection.NORTH, DiagonalBlockV2.NORTH, EightWayDirection.EAST, DiagonalBlockV2.EAST, EightWayDirection.SOUTH, DiagonalBlockV2.SOUTH, EightWayDirection.WEST, DiagonalBlockV2.WEST, EightWayDirection.NORTH_EAST, DiagonalBlockV2.NORTH_EAST, EightWayDirection.SOUTH_EAST, DiagonalBlockV2.SOUTH_EAST, EightWayDirection.SOUTH_WEST, DiagonalBlockV2.SOUTH_WEST, EightWayDirection.NORTH_WEST, DiagonalBlockV2.NORTH_WEST));
    Int2ObjectMap<Map<EightWayDirection, VoxelShape>> CORNER_SHAPES_CACHE = new Int2ObjectOpenHashMap<>();
    Object2ObjectMap<DiagonalBlockV2, Map<EightWayDirection, VoxelShape>> CORNER_SHAPES_BLOCK_CACHE = new Object2ObjectOpenHashMap<>();

    static BlockState updateDiagonalProperties(DiagonalBlockV2 diagonalBlock, BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, EightWayDirection... eightWayDirections) {
        for (EightWayDirection eightWayDirection : eightWayDirections) {
            if (!eightWayDirection.isIntercardinal()) throw new IllegalArgumentException("direction must be intercardinal");
            BlockPos neighborBlockPos = blockPos.offset(eightWayDirection.getX(), eightWayDirection.getY(), eightWayDirection.getZ());
            BlockState neighborBlockState = levelAccessor.getBlockState(neighborBlockPos);
            boolean value = allowsDiagonalProperty(diagonalBlock, levelAccessor, blockPos, neighborBlockState, eightWayDirection);
            if (value) {
                DiagonalBlockV2 neighborDiagonalBlock = (DiagonalBlockV2) neighborBlockState.getBlock();
                value = allowsDiagonalProperty(neighborDiagonalBlock, levelAccessor, neighborBlockPos, blockState, eightWayDirection.getOpposite());
            }
            blockState = blockState.setValue(PROPERTY_BY_DIRECTION.get(eightWayDirection), value);
        }
        return blockState;
    }

    static boolean allowsDiagonalProperty(DiagonalBlockV2 diagonalBlock, LevelAccessor levelAccessor, BlockPos blockPos, BlockState neighborBlockState, EightWayDirection eightWayDirection) {
        return diagonalBlock.attachesDiagonallyTo(neighborBlockState) && isFreeForDiagonalProperty(diagonalBlock, levelAccessor, blockPos, eightWayDirection) && isNotCollidingWithNeighbors(diagonalBlock, levelAccessor, blockPos, eightWayDirection);
    }

    static boolean isFreeForDiagonalProperty(DiagonalBlockV2 diagonalBlock, LevelAccessor levelAccessor, BlockPos blockPos, EightWayDirection eightWayDirection) {
        for (EightWayDirection neighbor : eightWayDirection.getCardinalNeighbors()) {
            Direction direction = neighbor.toDirection();
            BlockPos neighborBlockPos = blockPos.relative(direction);
            BlockState neighborBlockState = levelAccessor.getBlockState(neighborBlockPos);
            boolean isSideSolid = neighborBlockState.isFaceSturdy(levelAccessor, neighborBlockPos, direction.getOpposite());
            // do not use block state values, they might not have been updated yet on the other block
            if (diagonalBlock.attachesDirectlyTo(neighborBlockState, isSideSolid, direction.getOpposite())) {
                return false;
            }
        }
        return true;
    }

    static boolean isNotCollidingWithNeighbors(DiagonalBlockV2 diagonalBlock, LevelAccessor levelAccessor, BlockPos blockPos, EightWayDirection eightWayDirection) {
        if (CORNER_SHAPES_BLOCK_CACHE.containsKey(diagonalBlock)) {
            Map<EightWayDirection, VoxelShape> shapes = CORNER_SHAPES_BLOCK_CACHE.get(diagonalBlock);
            for (EightWayDirection neighbor : eightWayDirection.getCardinalNeighbors()) {
                VoxelShape cornerShape = shapes.get(eightWayDirection.data2d != neighbor.data2d ? eightWayDirection.rotateClockWise(2) : eightWayDirection.rotateCounterClockWise(2));
                BlockPos neighborBlockPos = blockPos.relative(neighbor.toDirection());
                BlockState neighborBlockState = levelAccessor.getBlockState(neighborBlockPos);
                VoxelShape voxelShape = neighborBlockState.getCollisionShape(levelAccessor, neighborBlockPos);
                if (Shapes.joinIsNotEmpty(cornerShape, voxelShape, BooleanOp.AND)) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    default VoxelShape[] _makeShapes(float nodeWidth, float extensionWidth, float nodeTop, float extensionBottom, float extensionTop) {
        if (extensionBottom == 0.0F && extensionTop <= 16.0F && !CORNER_SHAPES_BLOCK_CACHE.containsKey(this)) {
            Map<EightWayDirection, VoxelShape> cornerShapes = CORNER_SHAPES_CACHE.computeIfAbsent(Arrays.hashCode(new float[]{extensionWidth, extensionBottom, extensionTop}), $ -> {
                return Stream.of(EightWayDirection.getIntercardinalDirections()).collect(Maps.<EightWayDirection, EightWayDirection, VoxelShape>toImmutableEnumMap(Function.identity(), eightWayDirection -> {
                    return this.getCornerShape(eightWayDirection, extensionWidth, extensionBottom, extensionTop);
                }));
            });
            CORNER_SHAPES_BLOCK_CACHE.put(this, cornerShapes);
        }
        return StarShapeProvider.super._makeShapes(nodeWidth, extensionWidth, nodeTop, extensionBottom, extensionTop);
    }

    private VoxelShape getCornerShape(EightWayDirection eightWayDirection, float extensionWidth, float extensionBottom, float extensionTop) {
        int posX = eightWayDirection.getX() > 0 ? 0 : 16;
        int posZ = eightWayDirection.getZ() > 0 ? 0 : 16;
        return Block.box(posX - extensionWidth, extensionBottom, posZ - extensionWidth, posX + extensionWidth, extensionTop, posZ + extensionWidth);
    }

    @Override
    default boolean attachesDiagonallyTo(BlockState blockState) {
        return blockState.getBlock() instanceof DiagonalBlockV2 diagonalBlock && diagonalBlock.getType() == this.getType();
    }

    default BlockState addDefaultStates(BlockState defaultState) {
        return defaultState.setValue(DiagonalBlockV2.NORTH_EAST, Boolean.FALSE).setValue(DiagonalBlockV2.SOUTH_EAST, Boolean.FALSE).setValue(DiagonalBlockV2.SOUTH_WEST, Boolean.FALSE).setValue(DiagonalBlockV2.NORTH_WEST, Boolean.FALSE);
    }

    /**
     * See {@link Block#createBlockStateDefinition(StateDefinition.Builder)}.
     */
    default void _createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(DiagonalBlockV2.NORTH_EAST, DiagonalBlockV2.SOUTH_EAST, DiagonalBlockV2.SOUTH_WEST, DiagonalBlockV2.NORTH_WEST);
    }

    /**
     * See {@link Block#getStateForPlacement(BlockPlaceContext)}.
     */
    default BlockState _getStateForPlacement(BlockPlaceContext context, BlockState blockState) {
        Level level = context.getLevel();
        BlockPos clickedPos = context.getClickedPos();
        EightWayDirection[] eightWayDirections = EightWayDirection.getIntercardinalDirections();
        return updateDiagonalProperties(this, blockState, level, clickedPos, eightWayDirections);
    }

    /**
     * See {@link Block#updateShape(BlockState, Direction, BlockState, LevelAccessor, BlockPos, BlockPos)}.
     */
    default BlockState _updateShape(BlockState blockState, Direction direction, BlockState neighboringBlockState, LevelAccessor levelAccessor, BlockPos blockPos, BlockPos neighboringBlockPos) {
        if (direction.getAxis().getPlane() == Direction.Plane.HORIZONTAL) {
            EightWayDirection[] eightWayDirections = EightWayDirection.toEightWayDirection(direction).getIntercardinalNeighbors();
            return updateDiagonalProperties(this, blockState, levelAccessor, blockPos, eightWayDirections);
        }
        return blockState;
    }

    /**
     * See {@link Block#updateIndirectNeighbourShapes(BlockState, LevelAccessor, BlockPos, int, int)}.
     */
    default void _updateIndirectNeighbourShapes(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, int flags, int recursionLeft) {
        for (EightWayDirection eightWayDirection : EightWayDirection.getIntercardinalDirections()) {
            if (blockState.getValue(PROPERTY_BY_DIRECTION.get(eightWayDirection))) {
                BlockPos neighborBlockPos = blockPos.offset(eightWayDirection.getX(), eightWayDirection.getY(), eightWayDirection.getZ());
                BlockState neighborBlockState = levelAccessor.getBlockState(neighborBlockPos);
                BlockState newNeighborBlockState;
                if (neighborBlockState.getBlock() instanceof StarCollisionBlock starCollisionBlock) {
                    newNeighborBlockState = starCollisionBlock.updateIndirectNeighborDiagonalProperty(neighborBlockState, levelAccessor, neighborBlockPos, eightWayDirection.getOpposite());
                } else if (neighborBlockState.getBlock() instanceof DiagonalBlockV2 diagonalBlock) {
                    newNeighborBlockState = updateDiagonalProperties(diagonalBlock, neighborBlockState, levelAccessor, neighborBlockPos, eightWayDirection.getOpposite());
                } else {
                    newNeighborBlockState = null;
                }
                if (newNeighborBlockState != null) {
                    Block.updateOrDestroy(neighborBlockState, newNeighborBlockState, levelAccessor, neighborBlockPos, flags, recursionLeft);
                }
            }
        }
    }

    default BlockState updateIndirectNeighborDiagonalProperty(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, EightWayDirection eightWayDirection) {
        return updateDiagonalProperties(this, blockState, levelAccessor, blockPos, eightWayDirection);
    }
}
