package fuzs.diagonalfences.client.core;

import fuzs.diagonalfences.client.model.MultipartSegmentBakedModelFabric;
import fuzs.diagonalfences.mixin.client.accessor.ForwardingBakedModelAccessor;
import net.fabricmc.fabric.api.renderer.v1.model.ForwardingBakedModel;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public class FabricClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelFabric<>(baseModel, quadMap);
    }

    @Override
    public Optional<MultiPartBakedModel> getMultiPartBakedModel(BakedModel model) {
        if (model instanceof MultiPartBakedModel multiPartBakedModel) {
            return Optional.of(multiPartBakedModel);
        }
        if (model instanceof ForwardingBakedModel) {
            if (((ForwardingBakedModelAccessor) model).getWrapped() instanceof MultiPartBakedModel multiPartBakedModel) {
                return Optional.of(multiPartBakedModel);
            }
        }
        return Optional.empty();
    }
}
