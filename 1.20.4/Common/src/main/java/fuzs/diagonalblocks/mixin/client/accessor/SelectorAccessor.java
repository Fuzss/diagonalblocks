package fuzs.diagonalblocks.mixin.client.accessor;

import net.minecraft.client.renderer.block.model.multipart.Condition;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(Selector.class)
public interface SelectorAccessor {

    @Accessor("condition")
    Condition diagonalfences$getCondition();
}
