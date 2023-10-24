package fuzs.diagonalfences.api.v2.client;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.client.resources.model.MultipartAppender;
import fuzs.diagonalfences.mixin.client.accessor.MultiPartAccessor;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;

import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

public class MultiPartTranslator {
    private static final Map<DiagonalBlockType, MultiPartTranslator> TRANSLATORS = Maps.newConcurrentMap();
    private static final MultiPartTranslator IDENTITY = new MultiPartTranslator();

    public static void register(DiagonalBlockType diagonalBlockType, MultiPartTranslator translator) {
        if (TRANSLATORS.putIfAbsent(diagonalBlockType, translator) != null) {
            throw new IllegalStateException("duplicate multi part translator for diagonal block type '%s'".formatted(diagonalBlockType));
        }
    }

    public static MultiPartTranslator get(DiagonalBlockType diagonalBlockType) {
        return TRANSLATORS.getOrDefault(diagonalBlockType, IDENTITY);
    }

    public MultiPart apply(ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, MultiPart baseBlockModel, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        return this.applyAdditionalSelectors(modelAdder, this.getModelFromBase(modelLocation, diagonalBlockModel, baseBlockModel));
    }

    protected MultiPart getModelFromBase(ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, MultiPart baseBlockModel) {
        return this.makeMultiPart(modelLocation, diagonalBlockModel, Lists.newArrayList(baseBlockModel.getSelectors()));
    }

    protected MultiPart makeMultiPart(ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, List<Selector> selectors) {
        // we use a placeholder model that is provided via a runtime data generator, so the model bakery doesn't log a missing model
        // also the generated placeholder purposefully uses multipart, so we can reuse the stored state definition
        if (!(diagonalBlockModel instanceof MultiPart)) {
            throw new IllegalArgumentException("invalid model for diagonal block: " + modelLocation);
        }
        return new MultiPart(((MultiPartAccessor) diagonalBlockModel).diagonalfences$getDefinition(), selectors);
    }

    protected MultiPart applyAdditionalSelectors(BiConsumer<ResourceLocation, UnbakedModel> modelAdder, MultiPart multiPart) {
        return MultipartAppender.appendDiagonalSelectors(modelAdder, multiPart, false);
    }
}
