package com.fuzs.diagonalfences.block;

import com.fuzs.diagonalfences.util.EightWayDirection;
import com.fuzs.puzzleslib_df.util.math.shapes.NoneVoxelShape;
import com.fuzs.puzzleslib_df.util.math.shapes.VoxelCollection;
import com.fuzs.puzzleslib_df.util.math.shapes.VoxelUtils;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.SixWayBlock;
import net.minecraft.state.BooleanProperty;
import net.minecraft.state.StateContainer;
import net.minecraft.util.Direction;
import net.minecraft.util.Util;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.math.shapes.VoxelShapes;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.math.vector.Vector3i;
import net.minecraft.world.IWorld;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

public interface IEightWayBlock {

    BooleanProperty NORTH_EAST = BooleanProperty.create("north_east");
    BooleanProperty SOUTH_EAST = BooleanProperty.create("south_east");
    BooleanProperty SOUTH_WEST = BooleanProperty.create("south_west");
    BooleanProperty NORTH_WEST = BooleanProperty.create("north_west");

    Map<List<Float>, VoxelShape[]> DIMENSIONS_TO_SHAPE_MAP = Maps.newHashMap();
    Map<EightWayDirection, BooleanProperty> DIRECTION_TO_PROPERTY_MAP = Util.make(Maps.newEnumMap(EightWayDirection.class), (directions) -> {

        directions.put(EightWayDirection.NORTH, SixWayBlock.NORTH);
        directions.put(EightWayDirection.EAST, SixWayBlock.EAST);
        directions.put(EightWayDirection.SOUTH, SixWayBlock.SOUTH);
        directions.put(EightWayDirection.WEST, SixWayBlock.WEST);
        directions.put(EightWayDirection.NORTH_EAST, NORTH_EAST);
        directions.put(EightWayDirection.SOUTH_EAST, SOUTH_EAST);
        directions.put(EightWayDirection.SOUTH_WEST, SOUTH_WEST);
        directions.put(EightWayDirection.NORTH_WEST, NORTH_WEST);
    });

    boolean canConnectDiagonally(BlockState blockstate);

    default BlockState getDefaultStates(BlockState defaultState) {

        return defaultState.with(NORTH_EAST, Boolean.FALSE).with(SOUTH_EAST, Boolean.FALSE).with(SOUTH_WEST, Boolean.FALSE).with(NORTH_WEST, Boolean.FALSE);
    }

    default void fillStateContainer2(StateContainer.Builder<Block, BlockState> builder) {

        builder.add(NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST);
    }

    default int makeIndex(BlockState stateIn) {

        int index = 0;
        for (Map.Entry<EightWayDirection, BooleanProperty> entry : DIRECTION_TO_PROPERTY_MAP.entrySet()) {

            if (stateIn.get(entry.getValue())) {

                index |= entry.getKey().getHorizontalIndex();
            }
        }

        return index;
    }

    default BlockState updatePostPlacement2(BlockState stateIn, Direction facing, BlockState facingState, IWorld worldIn, BlockPos currentPos, BlockPos facingPos, BlockState newState) {

        if (facing.getAxis().getPlane() == Direction.Plane.HORIZONTAL) {

            BlockPos.Mutable diagonalPos = new BlockPos.Mutable();
            for (EightWayDirection direction : EightWayDirection.convert(facing).getIntercardinals()) {

                Vector3i directionVec = direction.getDirectionVec();
                diagonalPos.setAndOffset(currentPos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
                BlockState diagonalState = worldIn.getBlockState(diagonalPos);
                // checks if there are vertical connections where a diagonal connection should be formed
                boolean isBlocked = false;
                for (EightWayDirection cardinal : direction.getCardinals()) {

                    isBlocked = isBlocked || newState.get(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                newState = newState.with(DIRECTION_TO_PROPERTY_MAP.get(direction), !isBlocked && this.canConnectDiagonally(diagonalState));
            }

            return newState;
        }
        
        return null;
    }

    /**
     * similar to {@link net.minecraft.block.AbstractBlock.AbstractBlockState#updateNeighbours}
     */
    default void updateDiagonalNeighbors2(BlockState state, IWorld world, BlockPos pos, int flags, int recursionLeft) {

        BlockPos.Mutable diagonalPos = new BlockPos.Mutable();
        for (EightWayDirection direction : EightWayDirection.getAllIntercardinals()) {

            Vector3i directionVec = direction.getDirectionVec();
            diagonalPos.setAndOffset(pos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
            BlockState diagonalState = world.getBlockState(diagonalPos);
            if (diagonalState.getBlock() instanceof IEightWayBlock) {

                // checks if there are vertical connections where a diagonal connection should be formed
                boolean isBlocked = false;
                for (EightWayDirection cardinal : direction.opposite().getCardinals()) {

                    isBlocked = isBlocked || diagonalState.get(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                BlockState newState = diagonalState.with(DIRECTION_TO_PROPERTY_MAP.get(direction.opposite()), !isBlocked && ((IEightWayBlock) diagonalState.getBlock()).canConnectDiagonally(world.getBlockState(pos)));
                Block.replaceBlockState(diagonalState, newState, world, diagonalPos, flags, recursionLeft);
            }
        }
    }

    default VoxelShape[] getShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {

        ArrayList<Float> dimensions = Lists.newArrayList(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
        return DIMENSIONS_TO_SHAPE_MAP.computeIfAbsent(dimensions, dimension -> this.makeDiagonalShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight));
    }

    default VoxelCollection[] makeDiagonalShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {

        float nodeStart = 8.0F - nodeWidth;
        float nodeEnd = 8.0F + nodeWidth;
        float extensionStart = 8.0F - extensionWidth;
        float extensionEnd = 8.0F + extensionWidth;

        VoxelShape nodeShape = Block.makeCuboidShape(nodeStart, 0.0, nodeStart, nodeEnd, nodeHeight, nodeEnd);
        Vector3d[] sideShape = new Vector3d[]{new Vector3d(extensionStart, extensionBottom, 0.0), new Vector3d(extensionEnd, extensionHeight, extensionStart)};
        VoxelShape[] verticalShapes = Stream.of(EightWayDirection.values()).limit(4).map(direction -> direction.transform(sideShape)).map(VoxelUtils::makeCuboidShape).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalShapes = Stream.of(EightWayDirection.values()).skip(4).map(direction -> this.getDiagonalShape(extensionWidth, extensionBottom, extensionHeight, direction)).toArray(VoxelShape[]::new);
        VoxelShape[] sideShapes = new VoxelShape[]{verticalShapes[2], verticalShapes[3], verticalShapes[0], verticalShapes[1], diagonalShapes[2], diagonalShapes[3], diagonalShapes[0], diagonalShapes[1]};

        return this.constructStateShapes(nodeShape, sideShapes);
    }

    default VoxelCollection[] constructStateShapes(VoxelShape nodeShape, VoxelShape[] directionalShapes) {

        VoxelCollection[] stateShapes = new VoxelCollection[(int) Math.pow(2, directionalShapes.length)];
        for (int i = 0; i < stateShapes.length; i++) {

            stateShapes[i] = new VoxelCollection(nodeShape);
            for (int j = 0; j < directionalShapes.length; j++) {

                if ((i & (1 << j)) != 0) {

                    stateShapes[i].addVoxelShape(directionalShapes[j]);
                }
            }
        }

        return stateShapes;
    }

    default VoxelShape getDiagonalShape(float extensionWidth, float extensionBottom, float extensionHeight, EightWayDirection direction) {

        VoxelShape collisionShape = this.getDiagonalCollisionShape(extensionWidth, extensionBottom, extensionHeight, direction);
        // adept width for diagonal rotation
        extensionWidth = (float) Math.sqrt(extensionWidth * extensionWidth * 2);
        final float diagonalSide = 0.7071067812F * extensionWidth;
        Vector3d[] corners = VoxelUtils.createVectorArray(-diagonalSide, extensionHeight, diagonalSide, -diagonalSide + 8.0F, extensionHeight, diagonalSide + 8.0F, -diagonalSide, extensionBottom, diagonalSide, -diagonalSide + 8.0F, extensionBottom, diagonalSide + 8.0F, diagonalSide, extensionHeight, -diagonalSide, diagonalSide + 8.0F, extensionHeight, -diagonalSide + 8.0F, diagonalSide, extensionBottom, -diagonalSide, diagonalSide + 8.0F, extensionBottom, -diagonalSide + 8.0F);
        Vector3d[] edges = VoxelUtils.create12Edges(corners);
        if (direction.getDirectionVec().getX() != 1) {

            edges = VoxelUtils.flipX(edges);
        }

        if (direction.getDirectionVec().getZ() != 1) {

            edges = VoxelUtils.flipZ(edges);
        }

        return new NoneVoxelShape(collisionShape, VoxelUtils.scaleDown(edges));
    }

    default VoxelShape getDiagonalCollisionShape(float extensionWidth, float extensionBottom, float extensionHeight, EightWayDirection direction) {

        VoxelShape collisionShape = VoxelShapes.empty();
        for (int i = 0; i < 8; i++) {

            Vector3i directionVec = direction.getDirectionVec();
            int posX = directionVec.getX() > 0 ? i : 16 - i;
            int posZ = directionVec.getZ() > 0 ? i : 16 - i;
            VoxelShape cuboidShape = Block.makeCuboidShape(posX - extensionWidth, extensionBottom, posZ - extensionWidth, posX + extensionWidth, extensionHeight, posZ + extensionWidth);
            collisionShape = VoxelShapes.or(collisionShape, cuboidShape);
        }

        return collisionShape;
    }

}
