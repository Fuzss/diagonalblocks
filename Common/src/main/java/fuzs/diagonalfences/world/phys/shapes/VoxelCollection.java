package fuzs.diagonalfences.world.phys.shapes;

import com.google.common.collect.Lists;
import fuzs.diagonalfences.mixin.accessor.VoxelShapeAccessor;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.List;

public class VoxelCollection extends ExtensibleVoxelShape {
    private final List<NoneVoxelShape> noneVoxels = Lists.newArrayList();
    private VoxelShape collisionShape;
    private VoxelShape outlineShape;
    private VoxelShape particleShape;
    private boolean finished;

    public VoxelCollection() {
        this(Shapes.empty());
    }

    public VoxelCollection(VoxelShape baseShape) {
        super(baseShape);
        this.collisionShape = baseShape;
        this.outlineShape = baseShape;
        this.particleShape = baseShape;
    }

    @Override
    protected DoubleList getCoords(Direction.Axis axis) {
        return ((VoxelShapeAccessor) this.collisionShape).callGetCoords(axis);
    }

    public void addVoxelShape(VoxelShape voxelShape) {
        this.addVoxelShape(voxelShape, voxelShape);
    }

    public void addVoxelShape(VoxelShape voxelShape, VoxelShape particleShape) {
        if (this.finished) throw new IllegalStateException("Unable to modify VoxelCollection, already finished");
        if (voxelShape instanceof NoneVoxelShape) {
            this.addNoneVoxelShape((NoneVoxelShape) voxelShape);
        } else {
            this.setCollisionShape(Shapes.or(this.collisionShape, voxelShape));
            this.outlineShape = Shapes.or(this.outlineShape, voxelShape);
        }
        this.particleShape = Shapes.or(this.particleShape, particleShape);
    }

    private void addNoneVoxelShape(NoneVoxelShape voxelShape) {
        this.noneVoxels.add(voxelShape);
        // combine collision shapes
        this.setCollisionShape(Shapes.or(this.collisionShape, voxelShape));
    }

    private void setCollisionShape(VoxelShape voxelShape) {
        this.collisionShape = voxelShape;
        ((VoxelShapeAccessor) this).setShape(((VoxelShapeAccessor) this.collisionShape).getShape());
    }

    public void finish() {
        this.finished = true;
    }

    public void forAllParticleBoxes(Shapes.DoubleLineConsumer doubleLineConsumer) {
        this.particleShape.forAllBoxes(doubleLineConsumer);
    }

    @Override
    public void forAllEdges(Shapes.DoubleLineConsumer boxConsumer) {
        this.outlineShape.forAllEdges(boxConsumer);
        this.noneVoxels.forEach(voxelShape -> voxelShape.forAllEdges(boxConsumer));
    }
}
