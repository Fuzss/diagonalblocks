package fuzs.diagonalfences.client.model;

import net.minecraft.client.renderer.block.model.MultiVariant;
import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.Material;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.client.resources.model.ModelState;
import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.function.Function;

public class MultiVariantAdapter extends MultiVariant {
    private final BlockState state;
    private final Direction direction;

    public MultiVariantAdapter(List<Variant> list, BlockState state, Direction direction) {
        super(list);
        this.state = state;
        this.direction = direction;
    }

    @Nullable
    @Override
    public BakedModel bake(ModelBakery modelBakery, Function<Material, TextureAtlasSprite> spriteGetter, ModelState transform, ResourceLocation location) {
        // TODO cache this somehow, it's still being recalculated for every single block state which is really bad...
        return MultipartAppender.rotateMultipartSegment(this.state, super.bake(modelBakery, spriteGetter, transform, location), this.direction);
    }
}
