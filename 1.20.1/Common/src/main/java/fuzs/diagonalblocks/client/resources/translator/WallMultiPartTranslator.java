package fuzs.diagonalblocks.client.resources.translator;

import com.google.common.collect.Lists;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.mixin.client.accessor.KeyValueConditionAccessor;
import fuzs.diagonalblocks.mixin.client.accessor.SelectorAccessor;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.properties.WallSide;

import java.util.List;
import java.util.ListIterator;

public class WallMultiPartTranslator extends MultiPartTranslator {

    public WallMultiPartTranslator() {
        super(DiagonalBlockTypes.WALL);
    }

    @Override
    protected Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
        if (newProperty.getValueClass() == WallSide.class) {
            return (Boolean) oldValue ? WallSide.LOW : WallSide.NONE;
        } else {
            return super.getNewPropertyValue(oldProperty, newProperty, oldValue);
        }
    }

    @Override
    protected MultiPart getModelFromBase(Block diagonalBlock, MultiPart baseBlockModel) {
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
        return new MultiPart(diagonalBlock.getStateDefinition(), selectors);
    }

    @Override
    public boolean allowBaseModelAsFallback() {
        // since we change the block states on our wall block implementation from vanilla it is not safe to use the model of the vanilla wall base block
        return false;
    }
}
