package fuzs.diagonalblocks.client.resources.model;

import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.*;
import net.minecraft.core.Direction;

import java.util.function.Function;

public record RotatedVariant(Variant variant, Direction direction) implements UnbakedModel {

    @Override
    public void resolveDependencies(Resolver resolver) {
        resolver.resolve(this.variant.getModelLocation()).resolveDependencies(resolver);
    }

    @Override
    public BakedModel bake(ModelBaker baker, Function<Material, TextureAtlasSprite> spriteGetter, ModelState state) {
        return MultipartAppender.rotateMultipartSegment(null, baker.bake(this.variant.getModelLocation(), this.variant),
                this.direction
        );
    }
}
