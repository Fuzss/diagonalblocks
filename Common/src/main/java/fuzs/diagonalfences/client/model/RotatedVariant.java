package fuzs.diagonalfences.client.model;

import com.google.common.collect.Sets;
import com.mojang.datafixers.util.Pair;
import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.*;
import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.Set;
import java.util.function.Function;

public record RotatedVariant(Variant variant, Direction direction) implements UnbakedModel {

    @Override
    public Collection<ResourceLocation> getDependencies() {
        return Sets.newHashSet(this.variant.getModelLocation());
    }

    @Override
    public Collection<Material> getMaterials(Function<ResourceLocation, UnbakedModel> modelGetter, Set<Pair<String, String>> missingTextureErrors) {
        return modelGetter.apply(this.variant.getModelLocation()).getMaterials(modelGetter, missingTextureErrors);
    }

    @Nullable
    @Override
    public BakedModel bake(ModelBakery modelBakery, Function<Material, TextureAtlasSprite> spriteGetter, ModelState transform, ResourceLocation location) {
        return MultipartAppender.rotateMultipartSegment(null, modelBakery.bake(this.variant.getModelLocation(), this.variant), this.direction);
    }
}
