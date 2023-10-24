package fuzs.diagonalfences.client.resources.model;

import com.google.common.collect.Lists;
import fuzs.diagonalfences.mixin.client.accessor.KeyValueConditionAccessor;
import fuzs.diagonalfences.mixin.client.accessor.MultiPartAccessor;
import fuzs.diagonalfences.mixin.client.accessor.SelectorAccessor;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.state.properties.WallSide;

import java.util.List;
import java.util.ListIterator;

public interface MultiPartTranslator {
    MultiPartTranslator IDENTITY = (ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, MultiPart baseBlockModel) -> {
        List<Selector> selectors = Lists.newArrayList(baseBlockModel.getSelectors());
        return makeMultiPart(modelLocation, diagonalBlockModel, selectors);
    };
    MultiPartTranslator WALLS = (ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, MultiPart baseBlockModel) -> {
        List<Selector> selectors = Lists.newArrayList(baseBlockModel.getSelectors());
        ListIterator<Selector> iterator = selectors.listIterator();
        while (iterator.hasNext()) {
            Selector selector = iterator.next();
            if (((SelectorAccessor) selector).diagonalfences$getCondition() instanceof KeyValueCondition keyValueCondition) {
                String value = ((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getValue();
                if (value.equals(WallSide.LOW.toString())) {
                    value = Boolean.TRUE.toString();
                } else if (value.equals(WallSide.NONE.toString())) {
                    value = Boolean.FALSE.toString();
                } else if (value.equals(WallSide.TALL.toString())) {
                    value = null;
                } else {
                    continue;
                }
                if (value != null) {
                    String key = ((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getKey();
                    iterator.set(new Selector(new KeyValueCondition(key, value), selector.getVariant()));
                } else {
                    iterator.remove();
                }
            }
        }
        return makeMultiPart(modelLocation, diagonalBlockModel, selectors);
    };

    private static MultiPart makeMultiPart(ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, List<Selector> selectors) {
        // we use a placeholder model that is provided via a runtime data generator, so the model bakery doesn't log a missing model
        // also the generated placeholder purposefully uses multipart, so we can reuse the stored state definition
        if (!(diagonalBlockModel instanceof MultiPart)) {
            throw new IllegalArgumentException("invalid model for diagonal block: " + modelLocation);
        }
        return new MultiPart(((MultiPartAccessor) diagonalBlockModel).diagonalfences$getDefinition(), selectors);
    }

    MultiPart apply(ResourceLocation modelLocation, UnbakedModel diagonalBlockModel, MultiPart baseBlockModel);
}
