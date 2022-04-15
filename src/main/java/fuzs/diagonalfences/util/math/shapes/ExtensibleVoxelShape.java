package fuzs.diagonalfences.util.math.shapes;

import fuzs.diagonalfences.DiagonalFences;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.shapes.SliceShape;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraft.world.phys.shapes.DiscreteVoxelShape;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public abstract class ExtensibleVoxelShape extends SliceShape {

    private final Field partField;
    private final Method getValuesMethod;

    public ExtensibleVoxelShape(VoxelShape voxelProvider) {

        super(voxelProvider, Direction.Axis.X, 0);
        this.partField = ObfuscationReflectionHelper.findField(VoxelShape.class, "f_83211_");
        this.getValuesMethod = ObfuscationReflectionHelper.findMethod(VoxelShape.class, "m_7700_", Direction.Axis.class);
        this.setVoxelPart(this, this.getVoxelPart(voxelProvider));
    }

    @SuppressWarnings("NullableProblems")
    @Override
    protected abstract DoubleList getCoords(Direction.Axis axis);

    protected final void setVoxelPart(VoxelShape voxelShape, DiscreteVoxelShape part) {

        try {

            this.partField.set(voxelShape, part);
        } catch (IllegalAccessException ignored) {

            DiagonalFences.LOGGER.warn("Unable to set part field in {}", voxelShape.getClass().toString());
        }
    }

    protected final DiscreteVoxelShape getVoxelPart(VoxelShape voxelShape) {

        try {

            return (DiscreteVoxelShape) this.partField.get(voxelShape);
        } catch (IllegalAccessException ignored) {

            DiagonalFences.LOGGER.warn("Unable to get part field in {}", voxelShape.getClass().toString());
        }

        return null;
    }

    protected final DoubleList callGetValues(VoxelShape voxelShape, Direction.Axis axis) {

        try {

            return (DoubleList) this.getValuesMethod.invoke(voxelShape, axis);
        } catch (IllegalAccessException | InvocationTargetException ignored) {

            DiagonalFences.LOGGER.warn("Unable to call 'getValues' method in {}", voxelShape.getClass().toString());
        }

        return null;
    }

}
