package fuzs.diagonalfences.api.world.level.block;

import fuzs.diagonalfences.world.phys.shapes.VoxelUtils;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.util.StringRepresentable;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;

import java.util.Locale;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public enum EightWayDirection implements StringRepresentable {
    SOUTH(0, new Vec3i(0, 0, 1)),
    WEST(1, new Vec3i(-1, 0, 0)),
    NORTH(2, new Vec3i(0, 0, -1)),
    EAST(3, new Vec3i(1, 0, 0)),
    SOUTH_WEST(0, new Vec3i(-1, 0, 1)),
    NORTH_WEST(1, new Vec3i(-1, 0, -1)),
    NORTH_EAST(2, new Vec3i(1, 0, -1)),
    SOUTH_EAST(3, new Vec3i(1, 0, 1));

    public static final EightWayDirection[] CARDINAL_DIRECTIONS = Stream.of(EightWayDirection.values()).filter(EightWayDirection::isCardinal).toArray(EightWayDirection[]::new);
    public static final EightWayDirection[] INTERCARDINAL_DIRECTIONS = Stream.of(EightWayDirection.values()).filter(EightWayDirection::isIntercardinal).toArray(EightWayDirection[]::new);
    private static final Map<String, EightWayDirection> DIRECTIONS_BY_KEY = Stream.of(EightWayDirection.values()).collect(Collectors.toMap(EightWayDirection::getSerializedName, Function.identity()));

    private final int data2d;
    private final Vec3i directionVec;

    EightWayDirection(int data2d, Vec3i directionVec) {
        this.data2d = data2d;
        this.directionVec = directionVec;
    }

    public static EightWayDirection toEightWayDirection(Direction direction) {
        return CARDINAL_DIRECTIONS[direction.get2DDataValue()];
    }

    public static EightWayDirection byIndex(int index, boolean intercardinal) {
        return intercardinal ? INTERCARDINAL_DIRECTIONS[index % 4] : CARDINAL_DIRECTIONS[index % 4];
    }

    @Nullable
    public static EightWayDirection byName(String name) {
        return DIRECTIONS_BY_KEY.get(name);
    }

    public Vec3i directionVec() {
        return this.directionVec;
    }

    public boolean isCardinal() {
        return !this.isIntercardinal();
    }

    public boolean isIntercardinal() {
        return this.directionVec.getX() != 0 && this.directionVec.getZ() != 0;
    }

    public int getHorizontalIndex() {
        return 1 << (this.isIntercardinal() ? 4 + this.data2d : this.data2d);
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
        return EightWayDirection.byIndex((this.data2d + 2), this.isIntercardinal());
    }

    public EightWayDirection[] getCardinalNeighbors() {
        if (this.isIntercardinal()) {
            return new EightWayDirection[]{EightWayDirection.byIndex(this.data2d, false), EightWayDirection.byIndex(this.data2d + 1, false)};
        }
        throw new IllegalStateException("Direction already is cardinal");
    }

    public EightWayDirection[] getIntercardinalNeighbors() {
        if (this.isCardinal()) {
            return new EightWayDirection[]{EightWayDirection.byIndex(this.data2d + 3, true), EightWayDirection.byIndex(this.data2d, true)};
        }
        throw new IllegalStateException("Direction already is intercardinal");
    }

    public Direction toDirection() {
        if (this.isCardinal()) {
            return Direction.from2DDataValue(this.data2d);
        }
        throw new IllegalStateException("Cannot convert intercardinal direction to vanilla direction");
    }

    public EightWayDirection rotateClockwise() {
        return byIndex(this.isIntercardinal() ? this.data2d + 1 : this.data2d, !this.isIntercardinal());
    }

    @Override
    public String getSerializedName() {
        return this.name().toLowerCase(Locale.ROOT);
    }
}
