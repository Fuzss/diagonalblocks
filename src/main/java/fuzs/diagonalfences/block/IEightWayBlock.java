package fuzs.diagonalfences.block;

import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.mojang.serialization.Decoder;
import com.mojang.serialization.Encoder;
import com.mojang.serialization.MapCodec;
import fuzs.diagonalfences.api.IDiagonalBlock;
import fuzs.diagonalfences.mixin.accessor.IStateContainerAccessor;
import fuzs.diagonalfences.state.ExposedStateContainerBuilder;
import fuzs.diagonalfences.util.EightWayDirection;
import fuzs.diagonalfences.util.math.shapes.NoneVoxelShape;
import fuzs.diagonalfences.util.math.shapes.VoxelCollection;
import fuzs.diagonalfences.util.math.shapes.VoxelUtils;
import fuzs.puzzleslib.util.PuzzlesUtil;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.PipeBlock;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.core.Direction;
import net.minecraft.core.BlockPos;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.Vec3;
import net.minecraft.core.Vec3i;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.LevelAccessor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.stream.Stream;

@SuppressWarnings("unused")
public interface IEightWayBlock extends IDiagonalBlock {

    Map<List<Float>, VoxelShape[]> DIMENSIONS_TO_SHAPE_MAP = Maps.newHashMap();
    Map<EightWayDirection, BooleanProperty> DIRECTION_TO_PROPERTY_MAP = PuzzlesUtil.make(Maps.newEnumMap(EightWayDirection.class), (directions) -> {

        directions.put(EightWayDirection.NORTH, PipeBlock.NORTH);
        directions.put(EightWayDirection.EAST, PipeBlock.EAST);
        directions.put(EightWayDirection.SOUTH, PipeBlock.SOUTH);
        directions.put(EightWayDirection.WEST, PipeBlock.WEST);
        directions.put(EightWayDirection.NORTH_EAST, IDiagonalBlock.NORTH_EAST);
        directions.put(EightWayDirection.SOUTH_EAST, IDiagonalBlock.SOUTH_EAST);
        directions.put(EightWayDirection.SOUTH_WEST, IDiagonalBlock.SOUTH_WEST);
        directions.put(EightWayDirection.NORTH_WEST, IDiagonalBlock.NORTH_WEST);
    });

    boolean canConnect(BlockGetter iblockreader, BlockPos position, BlockState state, Direction direction);

    default BlockState getDefaultStates(BlockState defaultState) {

        return defaultState.setValue(IDiagonalBlock.NORTH_EAST, Boolean.FALSE).setValue(IDiagonalBlock.SOUTH_EAST, Boolean.FALSE).setValue(IDiagonalBlock.SOUTH_WEST, Boolean.FALSE).setValue(IDiagonalBlock.NORTH_WEST, Boolean.FALSE);
    }

    default MapCodec<BlockState> makeLenientMapCodec(Supplier<BlockState> defaultState, ExposedStateContainerBuilder<Block, BlockState> builder, ExposedStateContainerBuilder<Block, BlockState> additionalBuilder) {

        MapCodec<BlockState> mapcodec = MapCodec.of(Encoder.empty(), Decoder.unit(defaultState));
        for (Map.Entry<String, Property<?>> entry : ImmutableSortedMap.copyOf(builder.properties).entrySet()) {

            // ignore states added by us, world gen structures will otherwise fail to generate when our states are missing
            if (!additionalBuilder.properties.containsKey(entry.getKey())) {

                mapcodec = IStateContainerAccessor.callSetPropertyCodec(mapcodec, defaultState, entry.getKey(), entry.getValue());
            }
        }

        return mapcodec;
    }

    default void fillStateContainer2(StateDefinition.Builder<Block, BlockState> builder) {

        builder.add(IDiagonalBlock.NORTH_EAST, IDiagonalBlock.SOUTH_EAST, IDiagonalBlock.SOUTH_WEST, IDiagonalBlock.NORTH_WEST);
    }

    default int makeIndex(BlockState stateIn) {

        int index = 0;
        for (Map.Entry<EightWayDirection, BooleanProperty> entry : DIRECTION_TO_PROPERTY_MAP.entrySet()) {

            if (stateIn.getValue(entry.getValue())) {

                index |= entry.getKey().getHorizontalIndex();
            }
        }

        return index;
    }

    default BlockState makeStateForPlacement(BlockState placementState, BlockGetter blockGetter, BlockPos basePos, FluidState fluidState) {

        placementState.setValue(CrossCollisionBlock.WATERLOGGED, fluidState.getType() == Fluids.WATER);
        placementState = this.withDirections(EightWayDirection.getAllCardinals(), basePos, placementState, (mutablePos, newPlacementState, direction) ->
                this.canConnect(blockGetter, mutablePos, blockGetter.getBlockState(mutablePos), direction.convertTo().getOpposite()));
        placementState = this.withDirections(EightWayDirection.getAllIntercardinals(), basePos, placementState, (mutablePos, newPlacementState, direction) ->
                this.canConnectDiagonally(blockGetter.getBlockState(mutablePos)) && Stream.of(direction.getCardinalNeighbors()).map(DIRECTION_TO_PROPERTY_MAP::get).noneMatch(newPlacementState::getValue));

        return placementState;
    }

    default BlockState withDirections(EightWayDirection[] directions, BlockPos basePos, BlockState placementState, DirectionStatePredicate predicate) {

        BlockPos.MutableBlockPos mutablePos = new BlockPos.MutableBlockPos();
        for (EightWayDirection direction : directions) {

            Vec3i directionVec = direction.directionVec;
            mutablePos.setWithOffset(basePos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
            placementState = placementState.setValue(DIRECTION_TO_PROPERTY_MAP.get(direction), predicate.test(mutablePos, placementState, direction));
        }

        return placementState;
    }

    default BlockState updatePostPlacement2(BlockState state, Direction facing, BlockState facingState, LevelAccessor level, BlockPos currentPos, BlockPos facingPos, BlockState newState) {

        if (facing.getAxis().getPlane() == Direction.Plane.HORIZONTAL) {

            BlockPos.MutableBlockPos diagonalPos = new BlockPos.MutableBlockPos();
            for (EightWayDirection direction : EightWayDirection.convertTo(facing).getIntercardinalNeighbors()) {

                Vec3i directionVec = direction.directionVec;
                diagonalPos.setWithOffset(currentPos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
                BlockState diagonalState = level.getBlockState(diagonalPos);
                // checks if there are vertical connections where a diagonal connection should be formed
                boolean isBlocked = false;
                for (EightWayDirection cardinal : direction.getCardinalNeighbors()) {

                    isBlocked = isBlocked || newState.getValue(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                newState = newState.setValue(DIRECTION_TO_PROPERTY_MAP.get(direction), !isBlocked && this.canConnectDiagonally(diagonalState));
            }

            return newState;
        }
        
        return null;
    }

    /**
     * similar to {@link net.minecraft.world.level.block.state.BlockBehaviour.BlockStateBase#updateIndirectNeighbourShapes}
     */
    default void updateDiagonalNeighbors2(BlockState state, LevelAccessor level, BlockPos pos, int flags, int recursionLeft) {

        BlockPos.MutableBlockPos diagonalPos = new BlockPos.MutableBlockPos();
        for (EightWayDirection direction : EightWayDirection.getAllIntercardinals()) {

            Vec3i directionVec = direction.directionVec;
            diagonalPos.setWithOffset(pos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
            BlockState diagonalState = level.getBlockState(diagonalPos);
            if (diagonalState.getBlock() instanceof IEightWayBlock && ((IEightWayBlock) diagonalState.getBlock()).canConnectDiagonally()) {

                // checks if there are vertical connections where a diagonal connection should be formed
                boolean isBlocked = false;
                for (EightWayDirection cardinal : direction.getOpposite().getCardinalNeighbors()) {

                    isBlocked = isBlocked || diagonalState.getValue(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                BlockState newState = diagonalState.setValue(DIRECTION_TO_PROPERTY_MAP.get(direction.getOpposite()), !isBlocked && ((IEightWayBlock) diagonalState.getBlock()).canConnectDiagonally(level.getBlockState(pos)));
                Block.updateOrDestroy(diagonalState, newState, level, diagonalPos, flags, recursionLeft);
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

        VoxelShape nodeShape = Block.box(nodeStart, 0.0, nodeStart, nodeEnd, nodeHeight, nodeEnd);
        Vec3[] sideShape = new Vec3[]{new Vec3(extensionStart, extensionBottom, 0.0), new Vec3(extensionEnd, extensionHeight, extensionStart)};
        VoxelShape[] verticalShapes = Stream.of(EightWayDirection.getAllCardinals()).map(direction -> direction.transform(sideShape)).map(VoxelUtils::makeCuboidShape).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalShapes = Stream.of(EightWayDirection.getAllIntercardinals()).map(direction -> this.getDiagonalShape(extensionWidth, extensionBottom, extensionHeight, direction)).toArray(VoxelShape[]::new);
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
        // cos(-pi/4)
        final float diagonalSide = 0.7071067812F * extensionWidth;
        Vec3[] corners = VoxelUtils.createVectorArray(-diagonalSide, extensionHeight, diagonalSide, -diagonalSide + 8.0F, extensionHeight, diagonalSide + 8.0F, -diagonalSide, extensionBottom, diagonalSide, -diagonalSide + 8.0F, extensionBottom, diagonalSide + 8.0F, diagonalSide, extensionHeight, -diagonalSide, diagonalSide + 8.0F, extensionHeight, -diagonalSide + 8.0F, diagonalSide, extensionBottom, -diagonalSide, diagonalSide + 8.0F, extensionBottom, -diagonalSide + 8.0F);
        Vec3[] edges = VoxelUtils.create12Edges(corners);
        if (direction.directionVec.getX() != 1) {

            edges = VoxelUtils.flipX(edges);
        }

        if (direction.directionVec.getZ() != 1) {

            edges = VoxelUtils.flipZ(edges);
        }

        return new NoneVoxelShape(collisionShape, VoxelUtils.scaleDown(edges));
    }

    default VoxelShape getDiagonalCollisionShape(float extensionWidth, float extensionBottom, float extensionHeight, EightWayDirection direction) {

        VoxelShape collisionShape = Shapes.empty();
        for (int i = 0; i < 8; i++) {

            Vec3i directionVec = direction.directionVec;
            int posX = directionVec.getX() > 0 ? i : 16 - i;
            int posZ = directionVec.getZ() > 0 ? i : 16 - i;
            VoxelShape cuboidShape = Block.box(posX - extensionWidth, extensionBottom, posZ - extensionWidth, posX + extensionWidth, extensionHeight, posZ + extensionWidth);
            collisionShape = Shapes.or(collisionShape, cuboidShape);
        }

        return collisionShape;
    }

    @FunctionalInterface
    interface DirectionStatePredicate {

        boolean test(BlockPos pos, BlockState placementState, EightWayDirection direction);

    }

}
