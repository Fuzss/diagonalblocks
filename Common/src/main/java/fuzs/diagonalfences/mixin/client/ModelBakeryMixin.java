package fuzs.diagonalfences.mixin.client;

import fuzs.diagonalfences.client.model.MultipartAppender;
import net.minecraft.client.renderer.texture.AtlasSet;
import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.util.profiling.ProfilerFiller;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(ModelBakery.class)
abstract class ModelBakeryMixin {

    @Inject(method = "uploadTextures", at = @At(value = "FIELD", target = "Lnet/minecraft/client/resources/model/ModelBakery;topLevelModels:Ljava/util/Map;", shift = At.Shift.BEFORE))
    public void uploadTextures(TextureManager resourceManager, ProfilerFiller profiler, CallbackInfoReturnable<AtlasSet> callback) {
        MultipartAppender.onPrepareModelBaking(ModelBakery.class.cast(this));
    }
}
