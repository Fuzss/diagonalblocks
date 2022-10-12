package fuzs.diagonalfences.integration;

import dev.lambdaurora.lambdabettergrass.metadata.LBGCompiledLayerMetadata;
import dev.lambdaurora.lambdabettergrass.model.LBGLayerBakedModel;
import fuzs.diagonalfences.client.model.MultipartAppender;
import fuzs.puzzleslib.core.ReflectionHelperV2;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;

import java.util.List;

public class LambdaBetterGrassIntegration {

    public static void getMultiPartBakedModels(BakedModel model, List<MultipartAppender.MultiPartBakedModelMutator> list) {
        if (model instanceof LBGLayerBakedModel layerBakedModel) {
            ReflectionHelperV2.<List<LBGCompiledLayerMetadata>, LBGLayerBakedModel>getValue(LBGLayerBakedModel.class, "metadatas", layerBakedModel).ifPresent(metadatas -> {
                for (LBGCompiledLayerMetadata compiledLayerMetadata : metadatas) {
                    ReflectionHelperV2.<BakedModel, LBGCompiledLayerMetadata>getValue(LBGCompiledLayerMetadata.class, "bakedAlternateModel", compiledLayerMetadata).ifPresent(bakedAlternateModel -> {
                        if (bakedAlternateModel instanceof MultiPartBakedModel multiPartBakedModel) {
                            list.add(new MultipartAppender.MultiPartBakedModelMutator(multiPartBakedModel, newModel -> ReflectionHelperV2.setValue(LBGCompiledLayerMetadata.class, "bakedAlternateModel", compiledLayerMetadata, newModel)));
                        }
                    });
                }
            });
        }
    }
}
