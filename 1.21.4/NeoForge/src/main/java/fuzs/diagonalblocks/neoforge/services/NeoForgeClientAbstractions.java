package fuzs.diagonalblocks.neoforge.services;

import fuzs.diagonalblocks.neoforge.client.resources.model.NeoForgeMultipartSegmentBakedModel;
import fuzs.diagonalblocks.services.ClientAbstractions;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;

public final class NeoForgeClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel bakedModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new NeoForgeMultipartSegmentBakedModel(bakedModel, quadMap);
    }
}
