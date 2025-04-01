package fuzs.diagonalblocks.client.resources.translator;

import com.google.common.collect.Lists;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.resources.model.MultipartAppender;
import net.minecraft.client.renderer.block.model.BlockModelDefinition;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.properties.WallSide;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Optional;

public final class WallMultiPartTranslator extends MultiPartTranslator {

    public WallMultiPartTranslator() {
        super(DiagonalBlockTypes.WALL);
    }

    @Override
    protected Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
        if (newProperty.getValueClass() == WallSide.class && oldValue instanceof Boolean) {
            return (Boolean) oldValue ? WallSide.LOW : WallSide.NONE;
        } else {
            return super.getNewPropertyValue(oldProperty, newProperty, oldValue);
        }
    }

    @Override
    protected BlockModelDefinition.MultiPartDefinition getModelFromBase(BlockModelDefinition.MultiPartDefinition multiPart) {
        List<Selector> selectors = new ArrayList<>(multiPart.selectors());
        ListIterator<Selector> iterator = selectors.listIterator();
        while (iterator.hasNext()) {
            Selector selector = iterator.next();
            if (selector.condition().orElse(null) instanceof KeyValueCondition keyValueCondition) {
                String value = keyValueCondition.tests()
                        .entrySet()
                        .iterator()
                        .next()
                        .getValue()
                        .entries()
                        .getFirst()
                        .value();
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
                    String key = keyValueCondition.tests().entrySet().iterator().next().getKey();
                    iterator.set(new Selector(Optional.of(MultipartAppender.createKeyValueCondition(key, value)),
                            selector.variant()));
                } else {
                    iterator.remove();
                }
            }
        }

        return new BlockModelDefinition.MultiPartDefinition(selectors);
    }

    @Override
    public boolean allowBaseModelAsFallback() {
        // since we change the block states on our wall block implementation from vanilla it is not safe to use the model of the vanilla wall base block
        return false;
    }
}
