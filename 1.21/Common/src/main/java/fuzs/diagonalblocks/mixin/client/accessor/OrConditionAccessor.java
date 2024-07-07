package fuzs.diagonalblocks.mixin.client.accessor;

import net.minecraft.client.renderer.block.model.multipart.Condition;
import net.minecraft.client.renderer.block.model.multipart.OrCondition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(OrCondition.class)
public interface OrConditionAccessor {

    @Accessor("conditions")
    Iterable<? extends Condition> diagonalfences$getConditions();
}
