package fuzs.diagonalfences.core;

import fuzs.diagonalfences.world.phys.shapes.VoxelUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.util.StringRepresentable;
import net.minecraft.world.phys.Vec3;

/**
 * an extension to {@link Direction} to allow for intercardinals in the horizontal plane
 */
public enum EightWayDirection implements StringRepresentable {
    SOUTH("south", 0, new Vec3i(0, 0, 1)),
    WEST("west", 1, new Vec3i(-1, 0, 0)),
    NORTH("north", 2, new Vec3i(0, 0, -1)),
    EAST("east", 3, new Vec3i(1, 0, 0)),
    SOUTH_WEST("south_west", 0, new Vec3i(-1, 0, 1)),
    NORTH_WEST("north_west", 1, new Vec3i(-1, 0, -1)),
    NORTH_EAST("north_east", 2, new Vec3i(1, 0, -1)),
    SOUTH_EAST("south_east", 3, new Vec3i(1, 0, 1));

    private final String name;
    private final int data2d;
    private final Vec3i directionVec;

    EightWayDirection(String name, int data2d, Vec3i directionVec) {
        this.name = name;
        this.data2d = data2d;
        this.directionVec = directionVec;
    }

    @Override
    public String toString() {
        return this.name;
    }

    @Override
    public String getSerializedName() {
        return this.name;
    }

    public int getX() {
        return this.directionVec.getX();
    }

    public int getY() {
        return this.directionVec.getY();
    }

    public int getZ() {
        return this.directionVec.getZ();
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

    public boolean compareAxis(EightWayDirection other) {
        if (this.cardinal() != other.cardinal()) return false;
        return (this.getX() + other.getX() + this.getY() + other.getY() + this.getZ() + other.getZ()) == 0;
    }

    public EightWayDirection opposite() {
        return EightWayDirection.byIndex((this.data2d + 2), this.intercardinal());
    }

    public EightWayDirection[] getCardinalNeighbors() {
        if (!this.intercardinal()) throw new IllegalStateException("Direction already is cardinal");
        return new EightWayDirection[]{EightWayDirection.byIndex(this.data2d, false), EightWayDirection.byIndex(this.data2d + 1, false)};
    }

    public EightWayDirection[] getIntercardinalNeighbors() {
        if (!this.cardinal()) throw new IllegalStateException("Direction already is intercardinal");
        return new EightWayDirection[]{EightWayDirection.byIndex(this.data2d + 3, true), EightWayDirection.byIndex(this.data2d, true)};
    }

    public Direction toDirection() {
        if (!this.cardinal()) throw new IllegalStateException("Cannot convert intercardinal direction to vanilla direction");
        return Direction.from2DDataValue(this.data2d);
    }

    public BlockPos offset(BlockPos pos) {
        return pos.offset(this.directionVec);
    }

    public EightWayDirection getClockWise() {
        return byIndex(this.data2d + 1, this.intercardinal());
    }

    public EightWayDirection getCounterClockWise() {
        return byIndex(this.data2d - 1, this.intercardinal());
    }

    public static EightWayDirection toEightWayDirection(Direction direction) {
        return getCardinalDirections()[direction.get2DDataValue()];
    }

    public static EightWayDirection byIndex(int index, boolean intercardinal) {
        index = ((index % 4) + 4) % 4;
        return intercardinal ? getIntercardinalDirections()[index] : getCardinalDirections()[index];
    }

    public static EightWayDirection[] getCardinalDirections() {
        return new EightWayDirection[]{SOUTH, WEST, NORTH, EAST};
    }

    public static EightWayDirection[] getIntercardinalDirections() {
        return new EightWayDirection[]{SOUTH_WEST, NORTH_WEST, NORTH_EAST, SOUTH_EAST};
    }
}
