package com.fuzs.diagonalfences.util;

import com.fuzs.puzzleslib_df.util.math.shapes.VoxelUtils;
import net.minecraft.util.Direction;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.math.vector.Vector3i;

import java.util.Arrays;

public enum EightWayDirection {

    SOUTH(0, 1),
    WEST(-1, 0),
    NORTH(0, -1),
    EAST(1, 0),
    SOUTH_WEST(-1, 1),
    NORTH_WEST(-1, -1),
    NORTH_EAST(1, -1),
    SOUTH_EAST(1, 1);

    private final Vector3i directionVec;
    private final int horizontalIndex = 1 << this.ordinal();

    EightWayDirection(int directionX, int directionZ) {

        this.directionVec = new Vector3i(directionX, 0, directionZ);
    }

    public Vector3i getDirectionVec() {

        return this.directionVec;
    }

    public int getHorizontalIndex() {

        return this.horizontalIndex;
    }

    public Vector3d[] transform(Vector3d[] vectors) {

        if (this.directionVec.getX() != 0) {

            vectors = VoxelUtils.ortho(vectors);
        }

        if (this.directionVec.getX() == -1 || this.directionVec.getZ() == -1) {

            vectors = VoxelUtils.mirror(vectors);
        }

        return vectors;
    }

    public EightWayDirection opposite() {

        if (this.ordinal() >= 4) {

            return EightWayDirection.values()[(this.ordinal() + 2) % 4 + 4];
        }

        return EightWayDirection.values()[(this.ordinal() + 2) % 4];
    }

    public Direction convert() {

        return Direction.byHorizontalIndex(this.ordinal());
    }

    public static EightWayDirection[] getAllCardinals() {

        return Arrays.copyOf(EightWayDirection.values(), 4);
    }

    public static EightWayDirection[] getAllIntercardinals() {

        return Arrays.copyOfRange(EightWayDirection.values(), 4, 8);
    }

    public EightWayDirection[] getCardinals() {

        return new EightWayDirection[]{EightWayDirection.values()[this.ordinal() % 4], EightWayDirection.values()[(this.ordinal() + 1) % 4]};
    }

    public EightWayDirection[] getIntercardinals() {

        return new EightWayDirection[]{EightWayDirection.values()[(this.ordinal() + 3) % 4 + 4], EightWayDirection.values()[this.ordinal() + 4]};
    }

    public static EightWayDirection convert(Direction direction) {

        return EightWayDirection.values()[direction.getHorizontalIndex()];
    }

}
