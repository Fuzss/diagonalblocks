package fuzs.diagonalfences.util;

import fuzs.diagonalfences.util.math.shapes.VoxelUtils;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.world.phys.Vec3;

import java.util.stream.Stream;

/**
 * an extension to {@link Direction} to allow for intercardinals in the horizontal plane
 */
public enum EightWayDirection {
    SOUTH(0, new Vec3i(0, 0, 1)),
    WEST(1, new Vec3i(-1, 0, 0)),
    NORTH(2, new Vec3i(0, 0, -1)),
    EAST(3, new Vec3i(1, 0, 0)),
    SOUTH_WEST(0, new Vec3i(-1, 0, 1)),
    NORTH_WEST(1, new Vec3i(-1, 0, -1)),
    NORTH_EAST(2, new Vec3i(1, 0, -1)),
    SOUTH_EAST(3, new Vec3i(1, 0, 1));

    public static final EightWayDirection[] CARDINAL_DIRECTIONS = Stream.of(EightWayDirection.values())
            .filter(EightWayDirection::cardinal)
            .toArray(EightWayDirection[]::new);
    public static final EightWayDirection[] INTERCARDINAL_DIRECTIONS = Stream.of(EightWayDirection.values())
            .filter(EightWayDirection::intercardinal)
            .toArray(EightWayDirection[]::new);

    private final int data2d;
    private final Vec3i directionVec;

    EightWayDirection(int data2d, Vec3i directionVec) {
        this.data2d = data2d;
        this.directionVec = directionVec;
    }

    public Vec3i directionVec() {
        return this.directionVec;
    }

    public boolean cardinal() {
        return !this.intercardinal();
    }

    public boolean intercardinal() {
        return this.directionVec.getX() != 0 && this.directionVec.getZ() != 0;
    }

    public int getHorizontalIndex() {
        return 1 << (this.intercardinal() ? 4 + this.data2d : this.data2d);
    }

    public Vec3[] transform(Vec3[] vectors) {

        if (this.directionVec.getX() != 0) {

            vectors = VoxelUtils.ortho(vectors);
        }

        if (this.directionVec.getX() == -1 || this.directionVec.getZ() == -1) {

            vectors = VoxelUtils.mirror(vectors);
        }

        return vectors;
    }

    public EightWayDirection opposite() {
        return EightWayDirection.byIndex((this.data2d + 2), this.intercardinal());
    }

    public EightWayDirection[] getCardinalNeighbors() {
        if (!this.intercardinal()) {
            throw new IllegalStateException("Direction already is cardinal");
        }
        return new EightWayDirection[]{EightWayDirection.byIndex(this.data2d, false), EightWayDirection.byIndex(this.data2d + 1, false)};
    }

    public EightWayDirection[] getIntercardinalNeighbors() {
        if (!this.cardinal()) {
            throw new IllegalStateException("Direction already is intercardinal");
        }
        return new EightWayDirection[]{EightWayDirection.byIndex(this.data2d + 3, true), EightWayDirection.byIndex(this.data2d, true)};
    }

    public Direction toDirection() {
        if (!this.cardinal()) {
            throw new IllegalStateException("Cannot convert intercardinal direction to vanilla direction");
        }
        return Direction.from2DDataValue(this.data2d);
    }

    public static EightWayDirection toEightWayDirection(Direction direction) {
        return CARDINAL_DIRECTIONS[direction.get2DDataValue()];
    }

    public static EightWayDirection byIndex(int index, boolean intercardinal) {
        return intercardinal ? INTERCARDINAL_DIRECTIONS[index % 4] : CARDINAL_DIRECTIONS[index % 4];
    }
}
