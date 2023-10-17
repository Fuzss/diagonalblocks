package fuzs.diagonalfences.client;

import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.*;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.Nullable;

import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Function;

public record ForgeModelBakerImpl(Function<ResourceLocation, UnbakedModel> unbakedModelGetter,
                           Function<ResourceLocation, UnbakedModel> additionalUnbakedModelGetter,
                           Function<Material, TextureAtlasSprite> modelTextureGetter) implements ModelBaker {
    private static WeakReference<Map<ResourceLocation, AtlasSet.StitchResult>> atlasPreparationsReference = new WeakReference<>(null);

    public ForgeModelBakerImpl(ResourceLocation resourceLocation, Function<ResourceLocation, UnbakedModel> unbakedModelGetter, Function<ResourceLocation, UnbakedModel> additionalUnbakedModelGetter, BiConsumer<ResourceLocation, Material> missingTextureConsumer) {
        this(unbakedModelGetter, additionalUnbakedModelGetter, (Material material) -> {
            Map<ResourceLocation, AtlasSet.StitchResult> atlasPreparations = atlasPreparationsReference.get();
            Objects.requireNonNull(atlasPreparations, "atlas preparations is null");
            AtlasSet.StitchResult stitchResult = atlasPreparations.get(material.atlasLocation());
            TextureAtlasSprite textureatlassprite = stitchResult.getSprite(material.texture());
            if (textureatlassprite != null) {
                return textureatlassprite;
            } else {
                missingTextureConsumer.accept(resourceLocation, material);
                return stitchResult.missing();
            }
        });
    }

    public static void setAtlasPreparations(Map<ResourceLocation, AtlasSet.StitchResult> atlasPreparations) {
        ForgeModelBakerImpl.atlasPreparationsReference = new WeakReference<>(atlasPreparations);
    }

    @Override
    public UnbakedModel getModel(ResourceLocation resourceLocation) {
        UnbakedModel unbakedModel = this.additionalUnbakedModelGetter.apply(resourceLocation);
        if (unbakedModel != null) return unbakedModel;
        return this.unbakedModelGetter.apply(resourceLocation);
    }

    @Nullable
    @Override
    public BakedModel bake(ResourceLocation resourceLocation, ModelState modelState) {
        return this.bake(resourceLocation, modelState, this.modelTextureGetter);
    }

    @Override
    public @Nullable BakedModel bake(ResourceLocation resourceLocation, ModelState modelState, Function<Material, TextureAtlasSprite> modelTextureGetter) {
        return this.getModel(resourceLocation).bake(this, modelTextureGetter, modelState, resourceLocation);
    }

    @Override
    public Function<Material, TextureAtlasSprite> getModelTextureGetter() {
        return this.modelTextureGetter;
    }
}
