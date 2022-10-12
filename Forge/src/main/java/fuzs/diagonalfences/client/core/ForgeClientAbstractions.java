package fuzs.diagonalfences.client.core;

import fuzs.diagonalfences.client.model.MultipartSegmentBakedModelForge;
import fuzs.diagonalfences.mixin.client.accessor.BakedModelWrapperAccessor;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;
import net.minecraftforge.client.model.BakedModelWrapper;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public class ForgeClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelForge(baseModel, quadMap);
    }

    @Override
    public Optional<MultiPartBakedModel> getMultiPartBakedModels(BakedModel model) {
        if (model instanceof MultiPartBakedModel multiPartBakedModel) {
            return Optional.of(multiPartBakedModel);
        }
        if (model instanceof BakedModelWrapper<?>) {
            if (((BakedModelWrapperAccessor<?>) model).getOriginalModel() instanceof MultiPartBakedModel multiPartBakedModel) {
                return Optional.of(multiPartBakedModel);
            }
        }
        return Optional.empty();
    }
}
