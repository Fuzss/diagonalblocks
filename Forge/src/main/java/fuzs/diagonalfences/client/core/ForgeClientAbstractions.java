package fuzs.diagonalfences.client.core;

import com.google.common.collect.Lists;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.client.model.MultipartAppender;
import fuzs.diagonalfences.client.model.MultipartSegmentBakedModelForge;
import fuzs.diagonalfences.config.ClientConfig;
import fuzs.diagonalfences.mixin.client.accessor.BakedModelWrapperAccessor;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;
import net.minecraftforge.client.model.BakedModelWrapper;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public class ForgeClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelForge(baseModel, quadMap);
    }

    @Override
    public List<MultipartAppender.MultiPartBakedModelMutator> getMultiPartBakedModels(BakedModel model, Consumer<BakedModel> defaultConsumer) {
        List<MultipartAppender.MultiPartBakedModelMutator> list = Lists.newArrayList();
        if (model instanceof MultiPartBakedModel multiPartBakedModel) {
            list.add(new MultipartAppender.MultiPartBakedModelMutator(multiPartBakedModel, defaultConsumer));
        }
        if (DiagonalFences.CONFIG.get(ClientConfig.class).experimentalModIntegration && model instanceof BakedModelWrapper<?> modelWrapper) {
            if (((BakedModelWrapperAccessor<?>) model).getOriginalModel() instanceof MultiPartBakedModel multiPartBakedModel) {
                list.add(new MultipartAppender.MultiPartBakedModelMutator(multiPartBakedModel, getBakedModelWrapperConsumer(modelWrapper)));
            }
        }
        return list;
    }

    @SuppressWarnings("unchecked")
    private static <T extends BakedModel> Consumer<T> getBakedModelWrapperConsumer(BakedModelWrapper<? extends T> modelWrapper) {
        return newModel -> ((BakedModelWrapperAccessor<T>) modelWrapper).setOriginalModel(newModel);
    }
}
