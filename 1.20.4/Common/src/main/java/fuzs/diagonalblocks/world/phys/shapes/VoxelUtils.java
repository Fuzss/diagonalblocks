package fuzs.diagonalblocks.world.phys.shapes;

import fuzs.puzzleslib.api.shape.v1.ShapesHelper;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.stream.Stream;

public class VoxelUtils {

    public static Vec3[] scaleDown(Vec3[] edges) {
        return scale(edges, 0.0625);
    }

    public static Vec3[] scale(Vec3[] edges, double amount) {
        return Stream.of(edges).map(edge -> edge.scale(amount)).toArray(Vec3[]::new);
    }

    public static Vec3[] flipX(Vec3[] edges) {
        return Stream.of(edges).map(edge -> new Vec3(16.0 - edge.x, edge.y, edge.z)).toArray(Vec3[]::new);
    }

    public static Vec3[] flipZ(Vec3[] edges) {
        return Stream.of(edges).map(edge -> new Vec3(edge.x, edge.y, 16.0 - edge.z)).toArray(Vec3[]::new);
    }

    public static Vec3[] mirror(Vec3[] edges) {
        return flipZ(flipX(edges));
    }

    public static Vec3[] ortho(Vec3[] edges) {
        return Stream.of(edges).map(edge -> new Vec3(edge.z, edge.y, edge.x)).toArray(Vec3[]::new);
    }

    public static VoxelShape box(Vec3[] outline) {
        return ShapesHelper.box(outline[0].x, outline[0].y, outline[0].z, outline[1].x, outline[1].y, outline[1].z);
    }

    public static Vec3[] createVectorArray(Float... values) {
        return createVectorArray(Stream.of(values).map(Float::doubleValue).toArray(Double[]::new));
    }

    public static Vec3[] createVectorArray(Double... values) {
        if (values.length % 3 != 0) throw new IllegalStateException("Unable to create proper number of vectors");
        Vec3[] array = new Vec3[values.length / 3];
        for (int i = 0; i < array.length; i++) {
            int index = 3 * i;
            array[i] = new Vec3(values[index], values[index + 1], values[index + 2]);
        }
        return array;
    }

    /**
     * @param corners provided edges as top left, top right, bottom left and bottom right
     * @return vectors as pairs representing the edges
     */
    public static Vec3[] create12Edges(Vec3[] corners) {
        if (corners.length != 8) throw new IllegalStateException("Amount of corners must be 8");
        return new Vec3[]{
                // skew side
                corners[0], corners[1],
                corners[1], corners[3],
                corners[3], corners[2],
                corners[2], corners[0],
                // connections between skew sides
                corners[0], corners[4],
                corners[1], corners[5],
                corners[2], corners[6],
                corners[3], corners[7],
                // other skew side
                corners[4], corners[5],
                corners[5], corners[7],
                corners[7], corners[6],
                corners[6], corners[4]
        };
    }
}
