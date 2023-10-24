package fuzs.diagonalfences.client.resources.model;

import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.*;
import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.Collections;
import java.util.function.Function;

public record RotatedVariant(Variant variant, Direction direction) implements UnbakedModel {

    @Override
    public Collection<ResourceLocation> getDependencies() {
        return Collections.singleton(this.variant.getModelLocation());
    }

    @Override
    public void resolveParents(Function<ResourceLocation, UnbakedModel> function) {
        function.apply(this.variant.getModelLocation()).resolveParents(function);
    }

    @Nullable
    @Override
    public BakedModel bake(ModelBaker baker, Function<Material, TextureAtlasSprite> spriteGetter, ModelState state, ResourceLocation location) {
        return MultipartAppender.rotateMultipartSegment(null, baker.bake(this.variant.getModelLocation(), this.variant), this.direction);
    }
}
