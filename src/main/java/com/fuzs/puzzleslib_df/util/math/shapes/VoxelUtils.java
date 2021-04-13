package com.fuzs.puzzleslib_df.util.math.shapes;

import net.minecraft.block.Block;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.math.vector.Vector3d;

import java.util.stream.Stream;

public class VoxelUtils {

    public static Vector3d[] scaleDown(Vector3d[] edges) {

        return scale(edges, 0.0625);
    }

    public static Vector3d[] scale(Vector3d[] edges, double amount) {

        return Stream.of(edges).map(edge -> edge.scale(amount)).toArray(Vector3d[]::new);
    }

    public static Vector3d[] flipX(Vector3d[] edges) {

        return Stream.of(edges).map(edge -> new Vector3d(16.0 - edge.x, edge.y, edge.z)).toArray(Vector3d[]::new);
    }

    public static Vector3d[] flipZ(Vector3d[] edges) {

        return Stream.of(edges).map(edge -> new Vector3d(edge.x, edge.y, 16.0 - edge.z)).toArray(Vector3d[]::new);
    }

    public static Vector3d[] mirror(Vector3d[] edges) {

        return flipZ(flipX(edges));
    }

    public static Vector3d[] ortho(Vector3d[] edges) {

        return Stream.of(edges).map(edge -> new Vector3d(edge.z, edge.y, edge.x)).toArray(Vector3d[]::new);
    }

    public static VoxelShape makeCuboidShape(Vector3d[] outline) {

        Vector3d start = outline[0];
        Vector3d end = outline[1];

        return Block.makeCuboidShape(start.x, start.y, start.z, end.x, end.y, end.z);
    }

    public static Vector3d[] createVectorArray(Float... values) {

        return createVectorArray(Stream.of(values).map(Float::doubleValue).toArray(Double[]::new));
    }

    public static Vector3d[] createVectorArray(Double... values) {

        assert values.length % 3 == 0 : "Unable to create proper number of vectors";

        Vector3d[] array = new Vector3d[values.length / 3];
        for (int i = 0; i < array.length; i++) {

            int index = 3 * i;
            array[i] = new Vector3d(values[index], values[index + 1], values[index + 2]);
        }

        return array;
    }

    /**
     * @param corners provided edges as top left, top right, bottom left and bottom right
     * @return vectors as pairs representing the edges
     */
    public static Vector3d[] create12Edges(Vector3d[] corners) {

        assert corners.length == 8 : "Amount of corners must be 8";

        return new Vector3d[]{

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
