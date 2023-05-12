package fuzs.diagonalfences.mixin.client;

import fuzs.diagonalfences.client.model.MultipartAppender;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.Material;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.resources.ResourceLocation;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.function.BiFunction;

@Mixin(ModelBakery.class)
abstract class ModelBakeryMixin {

    @Inject(method = "bakeModels", at = @At("HEAD"))
    public void bakeModels(BiFunction<ResourceLocation, Material, TextureAtlasSprite> biFunction, CallbackInfo callback) {
        MultipartAppender.onPrepareModelBaking(ModelBakery.class.cast(this));
    }
}
