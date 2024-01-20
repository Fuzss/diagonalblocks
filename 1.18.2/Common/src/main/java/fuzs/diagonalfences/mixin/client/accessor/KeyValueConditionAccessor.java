package fuzs.diagonalfences.mixin.client.accessor;

import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(KeyValueCondition.class)
public interface KeyValueConditionAccessor {

    @Accessor("key")
    String diagonalfences$getKey();

    @Accessor("value")
    String diagonalfences$getValue();
}
