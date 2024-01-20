package fuzs.diagonalblocks.mixin.accessor;

import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.shapes.DiscreteVoxelShape;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Mutable;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.gen.Invoker;

@Mixin(VoxelShape.class)
public interface VoxelShapeAccessor {

    @Accessor("shape")
    DiscreteVoxelShape diagonalfences$getShape();

    @Accessor("shape")
    @Mutable
    void diagonalfences$setShape(DiscreteVoxelShape shape);

    @Invoker("getCoords")
    DoubleList diagonalfences$callGetCoords(Direction.Axis axis);
}
