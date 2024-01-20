package fuzs.diagonalfences.client.core;

import fuzs.puzzleslib.util.PuzzlesUtil;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;

public interface ClientAbstractions {
    ClientAbstractions INSTANCE = PuzzlesUtil.loadServiceProvider(ClientAbstractions.class);

    BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap);
}
