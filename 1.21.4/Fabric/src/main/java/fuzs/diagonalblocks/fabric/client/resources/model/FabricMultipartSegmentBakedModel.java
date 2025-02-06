package fuzs.diagonalblocks.fabric.client.resources.model;

import fuzs.diagonalblocks.client.resources.model.MultipartSegmentBakedModel;
import net.fabricmc.fabric.api.renderer.v1.mesh.QuadEmitter;
import net.fabricmc.fabric.api.renderer.v1.model.FabricBakedModel;
import net.fabricmc.fabric.impl.renderer.VanillaModelEncoder;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * Fabric adds a mixin in {@link net.minecraft.client.resources.model.DelegateBakedModel} that overrides
 * {@link FabricBakedModel#emitBlockQuads(QuadEmitter, BlockAndTintGetter, BlockState, BlockPos, Supplier, Predicate)}
 * and {@link FabricBakedModel#emitItemQuads(QuadEmitter, Supplier)}, which then call
 * {@link BakedModel#getQuads(BlockState, Direction, RandomSource)} on the wrapped model. We require the call to be on
 * this implementation though.
 */
public final class FabricMultipartSegmentBakedModel extends MultipartSegmentBakedModel {

    public FabricMultipartSegmentBakedModel(BakedModel bakedModel, Map<Direction, List<BakedQuad>> quadMap) {
        super(bakedModel, quadMap);
    }

    @Override
    public void emitBlockQuads(QuadEmitter emitter, BlockAndTintGetter blockView, BlockState state, BlockPos pos, Supplier<RandomSource> randomSupplier, Predicate<@Nullable Direction> cullTest) {
        VanillaModelEncoder.emitBlockQuads(emitter, (BakedModel) this, state, randomSupplier, cullTest);
    }

    @Override
    public void emitItemQuads(QuadEmitter emitter, Supplier<RandomSource> randomSupplier) {
        VanillaModelEncoder.emitItemQuads(emitter, (BakedModel) this, (BlockState) null, randomSupplier);
    }
}
