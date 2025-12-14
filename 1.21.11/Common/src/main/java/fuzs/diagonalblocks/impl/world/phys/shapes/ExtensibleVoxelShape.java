package fuzs.diagonalblocks.impl.world.phys.shapes;

import net.minecraft.core.Direction;
import net.minecraft.world.phys.shapes.SliceShape;
import net.minecraft.world.phys.shapes.VoxelShape;

public abstract class ExtensibleVoxelShape extends SliceShape {

    public ExtensibleVoxelShape(VoxelShape voxelProvider) {
        super(voxelProvider, Direction.Axis.X, 0);
        this.shape = voxelProvider.shape;
    }
}
