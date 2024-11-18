package fuzs.diagonalblocks.fabric.client.resources.model;

import net.fabricmc.fabric.api.renderer.v1.model.WrapperBakedModel;
import net.fabricmc.fabric.api.renderer.v1.render.RenderContext;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.ItemOverrides;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/**
 * We cannot use {@link net.fabricmc.fabric.api.renderer.v1.model.ForwardingBakedModel} as it overrides
 * {@link net.fabricmc.fabric.api.renderer.v1.model.FabricBakedModel#emitBlockQuads(BlockAndTintGetter, BlockState,
 * BlockPos, Supplier, RenderContext)} and
 * {@link net.fabricmc.fabric.api.renderer.v1.model.FabricBakedModel#emitItemQuads(ItemStack, Supplier, RenderContext)}
 * which then call {@link BakedModel#getQuads(BlockState, Direction, RandomSource)} on the original model, which is
 * incorrect.
 */
public record MultipartSegmentBakedModelFabric(BakedModel bakedModel,
                                               Map<Direction, List<BakedQuad>> quadMap) implements BakedModel, WrapperBakedModel {

    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, RandomSource rand) {
        return this.quadMap.get(side);
    }

    @Override
    public boolean useAmbientOcclusion() {
        return this.bakedModel.useAmbientOcclusion();
    }

    @Override
    public boolean isGui3d() {
        return this.bakedModel.isGui3d();
    }

    @Override
    public boolean usesBlockLight() {
        return this.bakedModel.usesBlockLight();
    }

    @Override
    public boolean isCustomRenderer() {
        return this.bakedModel.isCustomRenderer();
    }

    @Override
    public TextureAtlasSprite getParticleIcon() {
        return this.bakedModel.getParticleIcon();
    }

    @Override
    public ItemTransforms getTransforms() {
        return this.bakedModel.getTransforms();
    }

    @Override
    public ItemOverrides getOverrides() {
        return this.bakedModel.getOverrides();
    }

    @Override
    public boolean isVanillaAdapter() {
        return this.bakedModel.isVanillaAdapter();
    }

    @Override
    public BakedModel getWrappedModel() {
        return this.bakedModel;
    }
}
