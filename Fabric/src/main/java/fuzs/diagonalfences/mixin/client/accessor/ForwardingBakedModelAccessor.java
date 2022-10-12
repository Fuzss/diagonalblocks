package fuzs.diagonalfences.mixin.client.accessor;

import net.fabricmc.fabric.api.renderer.v1.model.ForwardingBakedModel;
import net.minecraft.client.resources.model.BakedModel;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(ForwardingBakedModel.class)
public interface ForwardingBakedModelAccessor {

    @Accessor
    BakedModel getWrapped();
}
