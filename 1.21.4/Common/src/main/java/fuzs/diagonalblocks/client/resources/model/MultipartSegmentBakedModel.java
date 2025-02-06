package fuzs.diagonalblocks.client.resources.model;

import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.DelegateBakedModel;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;

public class MultipartSegmentBakedModel extends DelegateBakedModel {
    protected final Map<Direction, List<BakedQuad>> quadMap;

    public MultipartSegmentBakedModel(BakedModel bakedModel, Map<Direction, List<BakedQuad>> quadMap) {
        super(bakedModel);
        this.quadMap = quadMap;
    }

    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction direction, RandomSource random) {
        return this.quadMap.get(direction);
    }
}
