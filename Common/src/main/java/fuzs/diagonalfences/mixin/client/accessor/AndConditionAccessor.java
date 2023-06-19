package fuzs.diagonalfences.mixin.client.accessor;

import net.minecraft.client.renderer.block.model.multipart.AndCondition;
import net.minecraft.client.renderer.block.model.multipart.Condition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(AndCondition.class)
public interface AndConditionAccessor {

    @Accessor("conditions")
    Iterable<? extends Condition> diagonalfences$getConditions();
}
