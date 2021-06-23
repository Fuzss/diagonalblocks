package fuzs.diagonalfences.util;

import fuzs.diagonalfences.util.math.shapes.VoxelUtils;
import net.minecraft.util.Direction;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.math.vector.Vector3i;

import java.util.stream.Stream;

public enum EightWayDirection {

    SOUTH(0, 0, 1),
    WEST(1, -1, 0),
    NORTH(2, 0, -1),
    EAST(3, 1, 0),
    SOUTH_WEST(0, -1, 1),
    NORTH_WEST(1, -1, -1),
    NORTH_EAST(2, 1, -1),
    SOUTH_EAST(3, 1, 1);

    public final int index;
    public final Vector3i directionVec;

    EightWayDirection(int index, int directionX, int directionZ) {

        this.index = index;
        this.directionVec = new Vector3i(directionX, 0, directionZ);
    }

    public boolean isCardinal() {

        return !this.isIntercardinal();
    }

    public boolean isIntercardinal() {

        return Math.abs(this.directionVec.getX()) + Math.abs(this.directionVec.getZ()) == 2;
    }

    public int getHorizontalIndex() {

        return 1 << (this.isIntercardinal() ? 4 + this.index : this.index);
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

    public EightWayDirection getOpposite() {

        return EightWayDirection.byIndex((this.index + 2), this.isIntercardinal());
    }

    public EightWayDirection[] getCardinalNeighbors() {

        assert this.isIntercardinal() : "Direction already is cardinal";
        return new EightWayDirection[]{EightWayDirection.byIndex(this.index, false), EightWayDirection.byIndex(this.index + 1, false)};
    }

    public EightWayDirection[] getIntercardinalNeighbors() {

        assert this.isCardinal() : "Direction already is intercardinal";
        return new EightWayDirection[]{EightWayDirection.byIndex(this.index + 3, true), EightWayDirection.byIndex(this.index, true)};
    }

    public Direction convertTo() {

        assert this.isCardinal() : "Cannot convert intercardinal direction to vanilla direction";
        return Direction.byHorizontalIndex(this.index);
    }

    public static EightWayDirection convertTo(Direction direction) {

        return EightWayDirection.getAllCardinals()[direction.getHorizontalIndex()];
    }

    public static EightWayDirection[] getAllCardinals() {

        return Stream.of(EightWayDirection.values())
                .filter(EightWayDirection::isCardinal)
                .toArray(EightWayDirection[]::new);
    }

    public static EightWayDirection[] getAllIntercardinals() {

        return Stream.of(EightWayDirection.values())
                .filter(EightWayDirection::isIntercardinal)
                .toArray(EightWayDirection[]::new);
    }

    public static EightWayDirection byIndex(int index, boolean intercardinal) {

        index %= 4;
        return intercardinal ? getAllIntercardinals()[index] : getAllCardinals()[index];
    }

    public static EightWayDirection byHorizontalIndex(int index) {

        return byIndex(index, index >= 4);
    }

}
