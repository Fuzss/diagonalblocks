package com.fuzs.diagonalfences.util.shape;

import com.fuzs.puzzleslib_df.util.math.shapes.ExtensibleVoxelShape;
import com.google.common.collect.Lists;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.util.Direction;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.math.shapes.VoxelShapes;
import net.minecraft.util.math.vector.Vector3d;

import java.util.List;

@SuppressWarnings("NullableProblems")
public class NoneVoxelShape extends ExtensibleVoxelShape {

    private final VoxelShape collisionShape;
    private final VoxelShape outlineShapeBase;
    private final List<Vector3d[]> outlineShapeEdges;

    public NoneVoxelShape(VoxelShape collisionShape, Vector3d... outlineShapeEdges) {

        this(collisionShape, VoxelShapes.empty(), outlineShapeEdges);
    }

    public NoneVoxelShape(VoxelShape collisionShape, VoxelShape outlineShapeBase, Vector3d... outlineShapeEdges) {

        super(collisionShape);

        this.collisionShape = collisionShape;
        this.outlineShapeBase = outlineShapeBase;
        this.outlineShapeEdges = this.createOutlineList(outlineShapeEdges);
    }

    private List<Vector3d[]> createOutlineList(Vector3d[] outlineShapeEdges) {

        assert outlineShapeEdges.length % 2 == 0 : "Edges must be in groups of two points";

        List<Vector3d[]> list = Lists.newArrayList();
        for (int i = 0; i < outlineShapeEdges.length; i += 2) {

            list.add(new Vector3d[]{outlineShapeEdges[i], outlineShapeEdges[i + 1]});
        }

        return list;
    }

    @Override
    protected DoubleList getValues(Direction.Axis axis) {

        return this.callGetValues(this.collisionShape, axis);
    }

    @Override
    public void forEachEdge(VoxelShapes.ILineConsumer boxConsumer) {

        this.outlineShapeBase.forEachEdge(boxConsumer);
        for (Vector3d[] edge : this.outlineShapeEdges) {

            boxConsumer.consume(edge[0].x, edge[0].y, edge[0].z, edge[1].x, edge[1].y, edge[1].z);
        }
    }

}
