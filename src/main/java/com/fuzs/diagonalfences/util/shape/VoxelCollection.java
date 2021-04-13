package com.fuzs.diagonalfences.util.shape;

import com.fuzs.diagonalfences.mixin.accessor.IVoxelShapeAccessor;
import com.google.common.collect.Lists;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.util.Direction;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.math.shapes.VoxelShapes;

import java.util.List;

public class VoxelCollection extends ExtensibleVoxelShape {

    private VoxelShape collisionShape;
    private VoxelShape outlineShape;
    private final List<NoneVoxelShape> noneVoxels = Lists.newArrayList();

    public VoxelCollection() {

        this(VoxelShapes.empty());
    }

    public VoxelCollection(VoxelShape baseShape) {

        super(baseShape);
        this.collisionShape = baseShape;
        this.outlineShape = baseShape;
    }

    @Override
    protected DoubleList getValues(Direction.Axis axis) {

        return ((IVoxelShapeAccessor) this.collisionShape).callGetValues(axis);
    }

    private void setCollisionShape(VoxelShape voxelShape) {

        this.collisionShape = voxelShape;
        this.setVoxelPart(((IVoxelShapeAccessor) this.collisionShape).getPart());
    }

    public void addVoxelShape(VoxelShape voxelShape) {

        if (voxelShape instanceof NoneVoxelShape) {

            this.addNoneVoxelShape((NoneVoxelShape) voxelShape);
        } else {

            this.setCollisionShape(VoxelShapes.or(this.collisionShape, voxelShape));
            this.outlineShape = VoxelShapes.or(this.outlineShape, voxelShape);
        }
    }

    private void addNoneVoxelShape(NoneVoxelShape voxelShape) {

        this.noneVoxels.add(voxelShape);
        // combine collision shapes
        this.setCollisionShape(VoxelShapes.or(this.collisionShape, voxelShape));
    }

    @SuppressWarnings("NullableProblems")
    @Override
    public void forEachEdge(VoxelShapes.ILineConsumer boxConsumer) {

        this.outlineShape.forEachEdge(boxConsumer);
        this.noneVoxels.forEach(voxelShape -> voxelShape.forEachEdge(boxConsumer));
    }

}