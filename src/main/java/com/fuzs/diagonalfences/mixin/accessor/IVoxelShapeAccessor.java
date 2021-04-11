package com.fuzs.diagonalfences.mixin.accessor;

import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.util.Direction;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.math.shapes.VoxelShapePart;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.gen.Invoker;

@Mixin(VoxelShape.class)
public interface IVoxelShapeAccessor {

    @Accessor
    VoxelShapePart getPart();

    @Accessor
    void setPart(VoxelShapePart part);

    @Invoker
    DoubleList callGetValues(Direction.Axis axis);

}
