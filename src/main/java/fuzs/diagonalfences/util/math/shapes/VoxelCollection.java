package fuzs.diagonalfences.util.math.shapes;

import com.google.common.collect.Lists;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraft.world.phys.shapes.Shapes;

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

        return this.callGetValues(this.collisionShape, axis);
    }

    private void setCollisionShape(VoxelShape voxelShape) {

        this.collisionShape = voxelShape;
        this.setVoxelPart(this, this.getVoxelPart(this.collisionShape));
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
