package fuzs.diagonalfences.world.level.block;

import com.google.common.base.Stopwatch;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.v2.DiagonalBlockV2;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.api.world.level.block.EightWayDirection;
import fuzs.diagonalfences.world.phys.shapes.NoneVoxelShape;
import fuzs.diagonalfences.world.phys.shapes.VoxelCollection;
import fuzs.diagonalfences.world.phys.shapes.VoxelUtils;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import net.minecraft.Util;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.PipeBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Stream;

public interface StarCollisionBlock extends DiagonalBlockV2 {
    /**
     * calculating shape unions is rather expensive, and since {@link VoxelShape} is immutable we use a cache for all diagonal blocks with the same shape
     */
    Int2ObjectMap<VoxelShape[]> DIMENSIONS_TO_SHAPE_CACHE = new Int2ObjectOpenHashMap<>();
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

    /**
     * sets default states for inter-cardinal properties
     * @param defaultState already modified default state obtained from {@link Block#defaultBlockState()}
     * @return state after setting inter-cardinal block states
     */
    default BlockState addDefaultStates(BlockState defaultState) {
        return defaultState.setValue(DiagonalBlock.NORTH_EAST, Boolean.FALSE).setValue(DiagonalBlock.SOUTH_EAST, Boolean.FALSE).setValue(DiagonalBlock.SOUTH_WEST, Boolean.FALSE).setValue(DiagonalBlock.NORTH_WEST, Boolean.FALSE);
    }

    default void _createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(DiagonalBlock.NORTH_EAST, DiagonalBlock.SOUTH_EAST, DiagonalBlock.SOUTH_WEST, DiagonalBlock.NORTH_WEST);
    }

    default int makeIndex(BlockState blockState) {
        int index = 0;
        for (Map.Entry<EightWayDirection, BooleanProperty> entry : DIRECTION_TO_PROPERTY_MAP.entrySet()) {
            if (blockState.getValue(entry.getValue())) {
                index |= entry.getKey().getHorizontalIndex();
            }
        }
        return index;
    }

    default BlockState _getStateForPlacement(BlockPlaceContext context, BlockState newBlockState) {
        Level level = context.getLevel();
        BlockPos clickedPos = context.getClickedPos();
        BlockPos.MutableBlockPos mutablePos = new BlockPos.MutableBlockPos();
        for (EightWayDirection direction : EightWayDirection.getIntercardinalDirections()) {
            boolean value = this.isFreeForDiagonals(newBlockState, direction);
            if (value) {
                mutablePos.setWithOffset(clickedPos, direction.getX(), direction.getY(), direction.getZ());
                value = this.attachesTo(newBlockState, level.getBlockState(mutablePos)) && this.isFreeForDiagonals(level.getBlockState(mutablePos), direction.opposite());
            }
            newBlockState = newBlockState.setValue(DIRECTION_TO_PROPERTY_MAP.get(direction), value);
        }
        return newBlockState;
    }
    
    private boolean isFreeForDiagonals(BlockState blockState, EightWayDirection direction) {
        for (EightWayDirection neighbor : direction.getCardinalNeighbors()) {
            if (blockState.getValue(DIRECTION_TO_PROPERTY_MAP.get(neighbor))) {
                return false;
            }
        }
        return true;
    }

    default BlockState _updateShape(BlockState blockState, Direction direction, BlockState neighboringBlockState, LevelAccessor levelAccessor, BlockPos blockPos, BlockPos neighboringBlockPos, BlockState newBlockState) {

        if (direction.getAxis().getPlane() == Direction.Plane.HORIZONTAL) {

            BlockPos.MutableBlockPos diagonalPos = new BlockPos.MutableBlockPos();
            for (EightWayDirection eightWayDirection : EightWayDirection.toEightWayDirection(direction).getIntercardinalNeighbors()) {

                diagonalPos.setWithOffset(blockPos, eightWayDirection.getX(), eightWayDirection.getY(), eightWayDirection.getZ());
                BlockState diagonalState = levelAccessor.getBlockState(diagonalPos);
                // checks if there are vertical connections where a diagonal connection should be formed
                boolean isBlocked = false;
                for (EightWayDirection cardinal : eightWayDirection.getCardinalNeighbors()) {

                    isBlocked |= newBlockState.getValue(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                newBlockState = newBlockState.setValue(DIRECTION_TO_PROPERTY_MAP.get(eightWayDirection), !isBlocked && this.canConnectToMe(diagonalState, eightWayDirection));
            }
        }
        
        return newBlockState;
    }

    /**
     * similar to {@link net.minecraft.world.level.block.state.BlockBehaviour.BlockStateBase#updateIndirectNeighbourShapes}
     */
    default void _updateIndirectNeighbourShapes(BlockState state, LevelAccessor level, BlockPos pos, int flags, int recursionLeft) {

        BlockPos.MutableBlockPos diagonalPos = new BlockPos.MutableBlockPos();

        for (EightWayDirection direction : EightWayDirection.getIntercardinalDirections()) {

            diagonalPos.setWithOffset(pos, direction.getX(), direction.getY(), direction.getZ());
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

        float[] dimensions = new float[]{nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight};
        return DIMENSIONS_TO_SHAPE_CACHE.computeIfAbsent(Arrays.hashCode(dimensions), $ -> this.makeDiagonalShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight));
    }

    default VoxelCollection[] makeDiagonalShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {

        Stopwatch stopwatch = Stopwatch.createStarted();

        float nodeStart = 8.0F - nodeWidth;
        float nodeEnd = 8.0F + nodeWidth;
        float extensionStart = 8.0F - extensionWidth;
        float extensionEnd = 8.0F + extensionWidth;

        VoxelShape nodeShape = Block.box(nodeStart, 0.0, nodeStart, nodeEnd, nodeHeight, nodeEnd);
        Vec3[] sideShape = new Vec3[]{new Vec3(extensionStart, extensionBottom, 0.0), new Vec3(extensionEnd, extensionHeight, nodeStart)};
        Vec3[] sideParticleShape = new Vec3[]{new Vec3(0.0, extensionBottom, 0.0), new Vec3(nodeStart, extensionHeight, nodeStart)};
        VoxelShape[] verticalShapes = Stream.of(EightWayDirection.getCardinalDirections()).map(direction -> direction.transform(sideShape)).map(VoxelUtils::makeCuboidShape).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalShapes = Stream.of(EightWayDirection.getIntercardinalDirections()).map(direction -> this.getDiagonalShape(extensionWidth, extensionBottom, extensionHeight, direction, nodeWidth == extensionWidth)).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalParticleShapes = Stream.of(EightWayDirection.getIntercardinalDirections()).map(direction -> {
            Vec3[] edges = sideParticleShape;
            if (direction.getX() != 1) {

                edges = VoxelUtils.flipX(edges);
            }

            if (direction.getZ() != 1) {

                edges = VoxelUtils.flipZ(edges);
            }
            return edges;
        }).map(VoxelUtils::makeCuboidShape).toArray(VoxelShape[]::new);
        VoxelShape[] sideShapes = new VoxelShape[]{verticalShapes[2], verticalShapes[3], verticalShapes[0], verticalShapes[1], diagonalShapes[2], diagonalShapes[3], diagonalShapes[0], diagonalShapes[1]};
        VoxelShape[] particleSideShapes = new VoxelShape[]{verticalShapes[2], verticalShapes[3], verticalShapes[0], verticalShapes[1], diagonalParticleShapes[2], diagonalParticleShapes[3], diagonalParticleShapes[0], diagonalParticleShapes[1]};

        VoxelCollection[] stateShapes = this.constructStateShapes(nodeShape, sideShapes, particleSideShapes);

        DiagonalFences.LOGGER.info("Constructing shapes for [NodeWith={},ExtensionWidth={},NodeHeight={},ExtensionBottom={},ExtensionHeight={}] took {}ms", nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight, stopwatch.stop().elapsed().toMillis());

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

    default VoxelShape getDiagonalShape(float extensionWidth, float extensionBottom, float extensionHeight, EightWayDirection direction, boolean stretchWidth) {

        VoxelShape collisionShape = this.getDiagonalCollisionShape(extensionWidth, extensionBottom, extensionHeight, direction);
        // are rotated extension shapes stretched in width to match the post shape
        if (stretchWidth) {
            extensionWidth = (float) Math.sqrt(extensionWidth * extensionWidth * 2);
        }
        // cos(-pi/4)
        final float diagonalSide = 0.7071067812F * extensionWidth;
        Vec3[] corners = VoxelUtils.createVectorArray(-diagonalSide, extensionHeight, diagonalSide, -diagonalSide + 8.0F, extensionHeight, diagonalSide + 8.0F, -diagonalSide, extensionBottom, diagonalSide, -diagonalSide + 8.0F, extensionBottom, diagonalSide + 8.0F, diagonalSide, extensionHeight, -diagonalSide, diagonalSide + 8.0F, extensionHeight, -diagonalSide + 8.0F, diagonalSide, extensionBottom, -diagonalSide, diagonalSide + 8.0F, extensionBottom, -diagonalSide + 8.0F);
        Vec3[] edges = VoxelUtils.create12Edges(corners);
        if (direction.getX() != 1) {

            edges = VoxelUtils.flipX(edges);
        }

        if (direction.getZ() != 1) {

            edges = VoxelUtils.flipZ(edges);
        }

        return new NoneVoxelShape(collisionShape, VoxelUtils.scaleDown(edges));
    }

    default VoxelShape getDiagonalCollisionShape(float extensionWidth, float extensionBottom, float extensionHeight, EightWayDirection direction) {

        VoxelShape collisionShape = Shapes.empty();
        for (int i = 0; i < 8; i++) {

            int posX = direction.getX() > 0 ? i : 16 - i;
            int posZ = direction.getZ() > 0 ? i : 16 - i;
            VoxelShape cuboidShape = Block.box(posX - extensionWidth, extensionBottom, posZ - extensionWidth, posX + extensionWidth, extensionHeight, posZ + extensionWidth);
            collisionShape = Shapes.or(collisionShape, cuboidShape);
        }

        return collisionShape;
    }
}
