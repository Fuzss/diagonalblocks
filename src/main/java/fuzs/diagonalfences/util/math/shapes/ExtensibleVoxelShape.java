package fuzs.diagonalfences.util.math.shapes;

import fuzs.diagonalfences.DiagonalFences;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.util.Direction;
import net.minecraft.util.math.shapes.SplitVoxelShape;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.math.shapes.VoxelShapePart;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public abstract class ExtensibleVoxelShape extends SplitVoxelShape {

    private final Field partField;
    private final Method getValuesMethod;

    public ExtensibleVoxelShape(VoxelShape voxelProvider) {

        super(voxelProvider, Direction.Axis.X, 0);
        this.partField = ObfuscationReflectionHelper.findField(VoxelShape.class, "field_197768_g");
        this.getValuesMethod = ObfuscationReflectionHelper.findMethod(VoxelShape.class, "func_197757_a", Direction.Axis.class);
        this.setVoxelPart(this, this.getVoxelPart(voxelProvider));
    }

    @SuppressWarnings("NullableProblems")
    @Override
    protected abstract DoubleList getValues(Direction.Axis axis);

    protected final void setVoxelPart(VoxelShape voxelShape, VoxelShapePart part) {

        try {

            this.partField.set(voxelShape, part);
        } catch (IllegalAccessException ignored) {

            DiagonalFences.LOGGER.warn("Unable to set part field in {}", voxelShape.getClass().toString());
        }
    }

    protected final VoxelShapePart getVoxelPart(VoxelShape voxelShape) {

        try {

            return (VoxelShapePart) this.partField.get(voxelShape);
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
