package fuzs.diagonalfences.client.core;

import fuzs.diagonalfences.client.resources.model.MultipartSegmentBakedModelFabric;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;

public class FabricClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelFabric<>(baseModel, quadMap);
    }
}
