package fuzs.diagonalblocks.api.v2.impl;

import com.google.common.base.Stopwatch;
import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.EightWayDirection;
import fuzs.diagonalblocks.world.phys.shapes.NoneVoxelShape;
import fuzs.diagonalblocks.world.phys.shapes.VoxelCollection;
import fuzs.diagonalblocks.world.phys.shapes.VoxelUtils;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.BooleanOp;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Stream;

public interface StarShapeProvider {
    Object2IntMap<BlockState> STATE_INDEX_CACHE = new Object2IntOpenHashMap<>();
    Int2ObjectMap<VoxelShape[]> SHAPES_CACHE = new Int2ObjectOpenHashMap<>();

    default int _getAABBIndex(BlockState state) {
        return STATE_INDEX_CACHE.computeIfAbsent(state, this::makeIndex);
    }

    default int makeIndex(BlockState blockState) {
        int index = 0;
        for (Map.Entry<EightWayDirection, BooleanProperty> entry : StarCollisionBlock.PROPERTY_BY_DIRECTION.entrySet()) {
            if (blockState.getValue(entry.getValue())) {
                index |= entry.getKey().getHorizontalIndex();
            }
        }
        return index;
    }

    default VoxelShape[] _makeShapes(float nodeWidth, float extensionWidth, float nodeTop, float extensionBottom, float extensionTop) {
        return SHAPES_CACHE.computeIfAbsent(Arrays.hashCode(new float[]{nodeWidth, extensionWidth, nodeTop, extensionBottom, extensionTop}), $ -> {
            return this.makeDiagonalShapes(nodeWidth, extensionWidth, nodeTop, extensionBottom, extensionTop);
        });
    }

    default VoxelShape[] makeDiagonalShapes(float nodeWidth, float extensionWidth, float nodeTop, float extensionBottom, float extensionTop) {

        Stopwatch stopwatch = Stopwatch.createStarted();

        float nodeStart = 8.0F - nodeWidth;
        float nodeEnd = 8.0F + nodeWidth;
        float extensionStart = 8.0F - extensionWidth;
        float extensionEnd = 8.0F + extensionWidth;

        VoxelShape nodeShape = Block.box(nodeStart, 0.0, nodeStart, nodeEnd, nodeTop, nodeEnd);
        Vec3[] sideShape = new Vec3[]{new Vec3(extensionStart, extensionBottom, 0.0), new Vec3(extensionEnd, extensionTop, nodeStart)};
        Vec3[] sideParticleShape = new Vec3[]{new Vec3(0.0, extensionBottom, 0.0), new Vec3(nodeStart, extensionTop, nodeStart)};
        VoxelShape[] verticalShapes = Stream.of(EightWayDirection.getCardinalDirections()).map(direction -> direction.transform(sideShape)).map(VoxelUtils::scaleDown).map(VoxelUtils::box).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalShapes = Stream.of(EightWayDirection.getIntercardinalDirections()).map(direction -> this.getDiagonalShape(extensionWidth, extensionBottom, extensionTop, direction, nodeWidth == extensionWidth)).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalParticleShapes = Stream.of(EightWayDirection.getIntercardinalDirections()).map(direction -> {

            Vec3[] edges = sideParticleShape;
            if (direction.getX() != 1) {

                edges = VoxelUtils.flipX(edges);
            }

            if (direction.getZ() != 1) {

                edges = VoxelUtils.flipZ(edges);
            }
            return edges;
        }).map(VoxelUtils::scaleDown).map(VoxelUtils::box).toArray(VoxelShape[]::new);
        VoxelShape[] sideShapes = new VoxelShape[]{verticalShapes[2], verticalShapes[3], verticalShapes[0], verticalShapes[1], diagonalShapes[2], diagonalShapes[3], diagonalShapes[0], diagonalShapes[1]};
        VoxelShape[] particleSideShapes = new VoxelShape[]{verticalShapes[2], verticalShapes[3], verticalShapes[0], verticalShapes[1], diagonalParticleShapes[2], diagonalParticleShapes[3], diagonalParticleShapes[0], diagonalParticleShapes[1]};

        VoxelShape[] stateShapes = this.constructStateShapes(nodeShape, sideShapes, particleSideShapes);

        DiagonalBlocks.LOGGER.info("Constructing shapes for {}[NodeWidth={},ExtensionWidth={},NodeTop={},ExtensionBottom={},ExtensionTop={}] took {}ms", this.getClass().getSimpleName(), nodeWidth, extensionWidth, nodeTop, extensionBottom, extensionTop, stopwatch.stop().elapsed().toMillis());

        return stateShapes;
    }

    default VoxelShape[] constructStateShapes(VoxelShape nodeShape, VoxelShape[] directionalShapes, VoxelShape[] particleDirectionalShapes) {

        VoxelCollection[] stateShapes = new VoxelCollection[(int) Math.pow(2, directionalShapes.length)];
        for (int i = 0; i < stateShapes.length; i++) {

            VoxelCollection voxelCollection = new VoxelCollection(nodeShape);
            for (int j = 0; j < directionalShapes.length; j++) {

                if ((i & (1 << j)) != 0) {

                    voxelCollection.addVoxelShape(directionalShapes[j], particleDirectionalShapes[j]);
                }
            }

            stateShapes[i] = voxelCollection.optimize();
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

    default VoxelShape getDiagonalCollisionShape(float extensionWidth, float extensionBottom, float extensionTop, EightWayDirection direction) {

        VoxelShape collisionShape = Shapes.empty();
        for (int i = 0; i < 8; i++) {

            int posX = direction.getX() > 0 ? i : 16 - i;
            int posZ = direction.getZ() > 0 ? i : 16 - i;
            VoxelShape cuboidShape = Block.box(posX - extensionWidth, extensionBottom, posZ - extensionWidth, posX + extensionWidth, extensionTop, posZ + extensionWidth);
            collisionShape = Shapes.joinUnoptimized(collisionShape, cuboidShape, BooleanOp.OR);
        }

        return collisionShape.optimize();
    }
}
