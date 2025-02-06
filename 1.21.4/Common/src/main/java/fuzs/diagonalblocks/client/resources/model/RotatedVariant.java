package fuzs.diagonalblocks.client.resources.model;

import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.block.model.TextureSlots;
import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelBaker;
import net.minecraft.client.resources.model.ModelState;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.Direction;

public record RotatedVariant(Variant variant, Direction direction) implements UnbakedModel {

    @Override
    public void resolveDependencies(Resolver resolver) {
        resolver.resolve(this.variant.modelLocation()).resolveDependencies(resolver);
    }

    @Override
    public BakedModel bake(TextureSlots textureSlots, ModelBaker baker, ModelState modelState, boolean hasAmbientOcclusion, boolean useBlockLight, ItemTransforms transforms) {
        return MultipartAppender.rotateMultipartSegment(null,
                baker.bake(this.variant.modelLocation(), this.variant),
                this.direction);
    }
}
