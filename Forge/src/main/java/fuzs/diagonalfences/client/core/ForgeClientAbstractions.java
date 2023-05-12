package fuzs.diagonalfences.client.core;

import fuzs.diagonalfences.client.model.MultipartSegmentBakedModelForge;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;

public class ForgeClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelForge(baseModel, quadMap);
    }
}
