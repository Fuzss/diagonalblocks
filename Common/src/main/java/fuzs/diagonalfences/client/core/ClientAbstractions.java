package fuzs.diagonalfences.client.core;

import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface ClientAbstractions {

    BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap);

    List<MultiPartBakedModel> getMultiPartBakedModels(BakedModel model);
}
