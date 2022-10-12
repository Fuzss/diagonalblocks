package fuzs.diagonalfences.client.core;

import fuzs.diagonalfences.client.model.MultipartAppender;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public interface ClientAbstractions {

    BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap);

    List<MultipartAppender.MultiPartBakedModelMutator> getMultiPartBakedModels(BakedModel model, Consumer<BakedModel> defaultConsumer);
}
