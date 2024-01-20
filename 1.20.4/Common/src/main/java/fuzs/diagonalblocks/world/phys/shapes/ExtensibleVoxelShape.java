package fuzs.diagonalblocks.world.phys.shapes;

import fuzs.diagonalblocks.mixin.accessor.VoxelShapeAccessor;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.shapes.SliceShape;
import net.minecraft.world.phys.shapes.VoxelShape;

public abstract class ExtensibleVoxelShape extends SliceShape {

    public ExtensibleVoxelShape(VoxelShape voxelProvider) {
        super(voxelProvider, Direction.Axis.X, 0);
        ((VoxelShapeAccessor) this).diagonalfences$setShape(((VoxelShapeAccessor) voxelProvider).diagonalfences$getShape());
    }

    @Override
    protected abstract DoubleList getCoords(Direction.Axis axis);
}
