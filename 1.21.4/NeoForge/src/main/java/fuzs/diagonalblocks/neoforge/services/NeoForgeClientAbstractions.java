package fuzs.diagonalblocks.neoforge.services;

import fuzs.diagonalblocks.neoforge.client.resources.model.MultipartSegmentBakedModelNeoForge;
import fuzs.diagonalblocks.services.ClientAbstractions;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;

public final class NeoForgeClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelNeoForge(baseModel, quadMap);
    }
}
