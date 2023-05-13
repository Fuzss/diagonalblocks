package fuzs.diagonalfences.world.level.block;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.api.world.level.block.EightWayDirection;
import fuzs.diagonalfences.world.phys.shapes.NoneVoxelShape;
import fuzs.diagonalfences.world.phys.shapes.VoxelCollection;
import fuzs.diagonalfences.world.phys.shapes.VoxelUtils;
import net.minecraft.Util;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.PipeBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.apache.commons.lang3.time.StopWatch;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

public interface StarCollisionBlock extends DiagonalBlock {
    /**
     * calculating shape unions is rather expensive, and since {@link VoxelShape} is immutable we use a cache for all diagonal blocks with the same shape
     */
    Map<List<Float>, VoxelShape[]> DIMENSIONS_TO_SHAPE_CACHE = Maps.newHashMap();
    Map<EightWayDirection, BooleanProperty> DIRECTION_TO_PROPERTY_MAP = Util.make(Maps.newEnumMap(EightWayDirection.class), (directions) -> {
        directions.put(EightWayDirection.NORTH, PipeBlock.NORTH);
        directions.put(EightWayDirection.EAST, PipeBlock.EAST);
        directions.put(EightWayDirection.SOUTH, PipeBlock.SOUTH);
        directions.put(EightWayDirection.WEST, PipeBlock.WEST);
        directions.put(EightWayDirection.NORTH_EAST, DiagonalBlock.NORTH_EAST);
        directions.put(EightWayDirection.SOUTH_EAST, DiagonalBlock.SOUTH_EAST);
        directions.put(EightWayDirection.SOUTH_WEST, DiagonalBlock.SOUTH_WEST);
        directions.put(EightWayDirection.NORTH_WEST, DiagonalBlock.NORTH_WEST);
    });

    boolean canConnect(BlockGetter blockGetter, BlockPos position, BlockState state, Direction direction);

    /**
     * sets default states for inter-cardinal properties
     * @param defaultState already modified default state obtained from {@link Block#defaultBlockState()}
     * @return state after setting inter-cardinal block states
     */
    default BlockState addDefaultStates(BlockState defaultState) {
        return defaultState.setValue(DiagonalBlock.NORTH_EAST, Boolean.FALSE).setValue(DiagonalBlock.SOUTH_EAST, Boolean.FALSE).setValue(DiagonalBlock.SOUTH_WEST, Boolean.FALSE).setValue(DiagonalBlock.NORTH_WEST, Boolean.FALSE);
    }

    default void createBlockStateDefinition2(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(DiagonalBlock.NORTH_EAST, DiagonalBlock.SOUTH_EAST, DiagonalBlock.SOUTH_WEST, DiagonalBlock.NORTH_WEST);
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

        placementState = placementState.setValue(CrossCollisionBlock.WATERLOGGED, fluidState.getType() == Fluids.WATER);

        placementState = this.withDirections(EightWayDirection.getCardinalDirections(), basePos, placementState, (mutablePos, newPlacementState, direction) ->
                this.canConnect(blockGetter, mutablePos, blockGetter.getBlockState(mutablePos), direction.toDirection().getOpposite()));

        placementState = this.withDirections(EightWayDirection.getIntercardinalDirections(), basePos, placementState, (pos, newPlacementState, direction) ->
                this.canConnectToMe(blockGetter.getBlockState(pos), direction.opposite()) && Stream.of(direction.getCardinalNeighbors()).map(DIRECTION_TO_PROPERTY_MAP::get).noneMatch(newPlacementState::getValue));

        return placementState;
    }

    default BlockState withDirections(EightWayDirection[] directions, BlockPos basePos, BlockState placementState, DirectionStatePredicate predicate) {

        BlockPos.MutableBlockPos mutablePos = new BlockPos.MutableBlockPos();
        for (EightWayDirection direction : directions) {

            Vec3i directionVec = direction.directionVec();
            mutablePos.setWithOffset(basePos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
            placementState = placementState.setValue(DIRECTION_TO_PROPERTY_MAP.get(direction), predicate.test(mutablePos, placementState, direction));
        }

        return placementState;
    }

    default BlockState updateShape2(BlockState state, Direction facing, BlockState facingState, LevelAccessor level, BlockPos currentPos, BlockPos facingPos, BlockState newState) {

        if (facing.getAxis().getPlane() == Direction.Plane.HORIZONTAL) {

            BlockPos.MutableBlockPos diagonalPos = new BlockPos.MutableBlockPos();
            for (EightWayDirection direction : EightWayDirection.toEightWayDirection(facing).getIntercardinalNeighbors()) {

                Vec3i directionVec = direction.directionVec();
                diagonalPos.setWithOffset(currentPos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
                BlockState diagonalState = level.getBlockState(diagonalPos);
                // checks if there are vertical connections where a diagonal connection should be formed
                boolean isBlocked = false;
                for (EightWayDirection cardinal : direction.getCardinalNeighbors()) {

                    isBlocked |= newState.getValue(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                newState = newState.setValue(DIRECTION_TO_PROPERTY_MAP.get(direction), !isBlocked && this.canConnectToMe(diagonalState, direction));
            }

            return newState;
        }
        
        return null;
    }

    /**
     * similar to {@link net.minecraft.world.level.block.state.BlockBehaviour.BlockStateBase#updateIndirectNeighbourShapes}
     */
    default void updateIndirectNeighbourShapes2(BlockState state, LevelAccessor level, BlockPos pos, int flags, int recursionLeft) {

        BlockPos.MutableBlockPos diagonalPos = new BlockPos.MutableBlockPos();

        for (EightWayDirection direction : EightWayDirection.getIntercardinalDirections()) {

            Vec3i directionVec = direction.directionVec();
            diagonalPos.setWithOffset(pos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
            BlockState diagonalState = level.getBlockState(diagonalPos);
            if (diagonalState.getBlock() instanceof StarCollisionBlock starCollisionBlock && starCollisionBlock.supportsDiagonalConnections()) {

                // checks if there are vertical connections where a diagonal connection should be formed
                boolean isBlocked = false;
                for (EightWayDirection cardinal : direction.opposite().getCardinalNeighbors()) {

                    isBlocked |= diagonalState.getValue(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                BlockState newState = diagonalState.setValue(DIRECTION_TO_PROPERTY_MAP.get(direction.opposite()), !isBlocked && starCollisionBlock.canConnectToMe(level.getBlockState(pos), direction));
                Block.updateOrDestroy(diagonalState, newState, level, diagonalPos, flags, recursionLeft);
            }
        }
    }

    default VoxelShape[] getShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {

        List<Float> dimensions = Lists.newArrayList(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
        return DIMENSIONS_TO_SHAPE_CACHE.computeIfAbsent(dimensions, dimension -> this.makeDiagonalShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight));
    }

    default VoxelCollection[] makeDiagonalShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {

        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        float nodeStart = 8.0F - nodeWidth;
        float nodeEnd = 8.0F + nodeWidth;
        float extensionStart = 8.0F - extensionWidth;
        float extensionEnd = 8.0F + extensionWidth;

        VoxelShape nodeShape = Block.box(nodeStart, 0.0, nodeStart, nodeEnd, nodeHeight, nodeEnd);
        Vec3[] sideShape = new Vec3[]{new Vec3(extensionStart, extensionBottom, 0.0), new Vec3(extensionEnd, extensionHeight, nodeStart)};
        Vec3[] sideParticleShape = new Vec3[]{new Vec3(0.0, extensionBottom, 0.0), new Vec3(nodeStart, extensionHeight, nodeStart)};
        VoxelShape[] verticalShapes = Stream.of(EightWayDirection.getCardinalDirections()).map(direction -> direction.transform(sideShape)).map(VoxelUtils::makeCuboidShape).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalShapes = Stream.of(EightWayDirection.getIntercardinalDirections()).map(direction -> this.getDiagonalShape(extensionWidth, extensionBottom, extensionHeight, direction)).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalParticleShapes = Stream.of(EightWayDirection.getIntercardinalDirections()).map(direction -> {
            Vec3[] edges = sideParticleShape;
            if (direction.directionVec().getX() != 1) {

                edges = VoxelUtils.flipX(edges);
            }

            if (direction.directionVec().getZ() != 1) {

                edges = VoxelUtils.flipZ(edges);
            }
            return edges;
        }).map(VoxelUtils::makeCuboidShape).toArray(VoxelShape[]::new);
        VoxelShape[] sideShapes = new VoxelShape[]{verticalShapes[2], verticalShapes[3], verticalShapes[0], verticalShapes[1], diagonalShapes[2], diagonalShapes[3], diagonalShapes[0], diagonalShapes[1]};
        VoxelShape[] particleSideShapes = new VoxelShape[]{verticalShapes[2], verticalShapes[3], verticalShapes[0], verticalShapes[1], diagonalParticleShapes[2], diagonalParticleShapes[3], diagonalParticleShapes[0], diagonalParticleShapes[1]};

        VoxelCollection[] stateShapes = this.constructStateShapes(nodeShape, sideShapes, particleSideShapes);

        stopWatch.stop();
        DiagonalFences.LOGGER.info("Constructing shapes for nodeWith {}, extensionWidth {}, nodeHeight {}, extensionBottom {}, extensionHeight {} took {}ms", nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight, stopWatch.getTime());

        return stateShapes;
    }

    default VoxelCollection[] constructStateShapes(VoxelShape nodeShape, VoxelShape[] directionalShapes, VoxelShape[] particleDirectionalShapes) {

        VoxelCollection[] stateShapes = new VoxelCollection[(int) Math.pow(2, directionalShapes.length)];
        for (int i = 0; i < stateShapes.length; i++) {

            stateShapes[i] = new VoxelCollection(nodeShape);
            for (int j = 0; j < directionalShapes.length; j++) {

                if ((i & (1 << j)) != 0) {

                    stateShapes[i].addVoxelShape(directionalShapes[j], particleDirectionalShapes[j]);
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
        if (direction.directionVec().getX() != 1) {

            edges = VoxelUtils.flipX(edges);
        }

        if (direction.directionVec().getZ() != 1) {

            edges = VoxelUtils.flipZ(edges);
        }

        return new NoneVoxelShape(collisionShape, VoxelUtils.scaleDown(edges));
    }

    default VoxelShape getDiagonalCollisionShape(float extensionWidth, float extensionBottom, float extensionHeight, EightWayDirection direction) {

        VoxelShape collisionShape = Shapes.empty();
        for (int i = 0; i < 8; i++) {

            int posX = direction.directionVec().getX() > 0 ? i : 16 - i;
            int posZ = direction.directionVec().getZ() > 0 ? i : 16 - i;
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
