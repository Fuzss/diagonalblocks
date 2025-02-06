package fuzs.diagonalblocks.neoforge.client.resources.model;

import fuzs.diagonalblocks.client.resources.model.MultipartSegmentBakedModel;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.model.data.ModelData;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;

public final class NeoForgeMultipartSegmentBakedModel extends MultipartSegmentBakedModel {

    public NeoForgeMultipartSegmentBakedModel(BakedModel bakedModel, Map<Direction, List<BakedQuad>> quadMap) {
        super(bakedModel, quadMap);
    }

    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState blockState, @Nullable Direction direction, RandomSource randomSource, ModelData modelData, @Nullable RenderType renderType) {
        return this.quadMap.get(direction);
    }
}
