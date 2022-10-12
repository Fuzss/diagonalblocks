package fuzs.diagonalfences.client.core;

import com.google.common.collect.Lists;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.client.model.MultipartAppender;
import fuzs.diagonalfences.client.model.MultipartSegmentBakedModelFabric;
import fuzs.diagonalfences.config.ClientConfig;
import fuzs.diagonalfences.integration.LambdaBetterGrassIntegration;
import fuzs.puzzleslib.core.CoreServices;
import fuzs.puzzleslib.core.ReflectionHelperV2;
import net.fabricmc.fabric.api.renderer.v1.model.ForwardingBakedModel;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public class FabricClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelFabric<>(baseModel, quadMap);
    }

    @Override
    public List<MultipartAppender.MultiPartBakedModelMutator> getMultiPartBakedModels(BakedModel model, Consumer<BakedModel> defaultConsumer) {
        List<MultipartAppender.MultiPartBakedModelMutator> list = Lists.newArrayList();
        if (model instanceof MultiPartBakedModel multiPartBakedModel) {
            list.add(new MultipartAppender.MultiPartBakedModelMutator(multiPartBakedModel, defaultConsumer));
        }
        if (DiagonalFences.CONFIG.get(ClientConfig.class).experimentalModIntegration && model instanceof ForwardingBakedModel forwardingBakedModel) {
            ReflectionHelperV2.getValue(ForwardingBakedModel.class, "wrapped", forwardingBakedModel).ifPresent(wrappedModel -> {
                if (wrappedModel instanceof MultiPartBakedModel multiPartBakedModel) {
                    list.add(new MultipartAppender.MultiPartBakedModelMutator(multiPartBakedModel, newModel -> ReflectionHelperV2.setValue(ForwardingBakedModel.class, "wrapped", forwardingBakedModel, newModel)));
                }
            });
            if (DiagonalFences.CONFIG.get(ClientConfig.class).lambdaBetterGrassIntegration && CoreServices.ENVIRONMENT.isModLoaded("lambdabettergrass")) {
                LambdaBetterGrassIntegration.getMultiPartBakedModels(model, list);
            }
        }
        return list;
    }
}
