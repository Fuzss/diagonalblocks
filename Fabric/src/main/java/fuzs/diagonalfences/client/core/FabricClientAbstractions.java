package fuzs.diagonalfences.client.core;

import com.google.common.collect.Lists;
import dev.lambdaurora.lambdabettergrass.metadata.LBGCompiledLayerMetadata;
import dev.lambdaurora.lambdabettergrass.model.LBGBakedModel;
import dev.lambdaurora.lambdabettergrass.model.LBGLayerBakedModel;
import fuzs.diagonalfences.client.model.MultipartSegmentBakedModelFabric;
import fuzs.diagonalfences.mixin.client.accessor.ForwardingBakedModelAccessor;
import fuzs.puzzleslib.core.ReflectionHelper;
import net.fabricmc.fabric.api.renderer.v1.model.ForwardingBakedModel;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class FabricClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelFabric<>(baseModel, quadMap);
    }

    @Override
    public List<MultiPartBakedModel> getMultiPartBakedModels(BakedModel model) {
        if (model instanceof MultiPartBakedModel multiPartBakedModel) {
            return Lists.newArrayList(multiPartBakedModel);
        }
        if (model instanceof ForwardingBakedModel) {
            List<MultiPartBakedModel> list = Lists.newArrayList();
            if (((ForwardingBakedModelAccessor) model).getWrapped() instanceof MultiPartBakedModel multiPartBakedModel) {
                list.add(multiPartBakedModel);
            }
            if (model instanceof LBGLayerBakedModel) {
                Field metadatas = ReflectionHelper.getDeclaredField(LBGLayerBakedModel.class, "metadatas");
                Optional<List<LBGCompiledLayerMetadata>> metadata = ReflectionHelper.<List<LBGCompiledLayerMetadata>>get(metadatas, model);
                metadata.ifPresent(m -> {
                    for (LBGCompiledLayerMetadata lbgCompiledLayerMetadata : m) {
                        Optional<BakedModel> bakedAlternateModel = get(ReflectionHelper.getDeclaredField(LBGCompiledLayerMetadata.class, "bakedAlternateModel"), lbgCompiledLayerMetadata);
                        bakedAlternateModel.ifPresent(model2 -> {
                            if (model2 instanceof MultiPartBakedModel multiPartBakedModel2) {
                                list.add(multiPartBakedModel2);
                            }
                        });
                    }
                });
            }
            return list;
        }
        return Lists.newArrayList();
    }

    public static <T> Optional<T> get(@Nullable Field field, Object instance) {
        if (field != null) {
            try {
                return Optional.ofNullable((T) field.get(instance));
            } catch (IllegalAccessException ignored) {
            }
        }
        return Optional.empty();
    }
}
