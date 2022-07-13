package fuzs.diagonalfences.util.math.shapes;

import com.google.common.collect.Lists;
import fuzs.diagonalfences.mixin.accessor.VoxelShapeAccessor;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.List;

public class VoxelCollection extends ExtensibleVoxelShape {
    private VoxelShape collisionShape;
    private VoxelShape outlineShape;
    private final List<NoneVoxelShape> noneVoxels = Lists.newArrayList();

    public VoxelCollection() {
        this(Shapes.empty());
    }

    public VoxelCollection(VoxelShape baseShape) {
        super(baseShape);
        this.collisionShape = baseShape;
        this.outlineShape = baseShape;
    }

    @Override
    protected DoubleList getCoords(Direction.Axis axis) {
        return ((VoxelShapeAccessor) this.collisionShape).callGetCoords(axis);
    }

    private void setCollisionShape(VoxelShape voxelShape) {
        this.collisionShape = voxelShape;
        ((VoxelShapeAccessor) this).setShape(((VoxelShapeAccessor) this.collisionShape).getShape());
    }

    public void addVoxelShape(VoxelShape voxelShape) {
        if (voxelShape instanceof NoneVoxelShape) {
            this.addNoneVoxelShape((NoneVoxelShape) voxelShape);
        } else {
            this.setCollisionShape(Shapes.or(this.collisionShape, voxelShape));
            this.outlineShape = Shapes.or(this.outlineShape, voxelShape);
        }
    }

    private void addNoneVoxelShape(NoneVoxelShape voxelShape) {
        this.noneVoxels.add(voxelShape);
        // combine collision shapes
        this.setCollisionShape(Shapes.or(this.collisionShape, voxelShape));
    }

    @Override
    public void forAllEdges(Shapes.DoubleLineConsumer boxConsumer) {
        this.outlineShape.forAllEdges(boxConsumer);
        this.noneVoxels.forEach(voxelShape -> voxelShape.forAllEdges(boxConsumer));
    }
}
