package fuzs.diagonalblocks.impl.client.resources.translator;

import com.google.common.collect.ImmutableList;
import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.impl.client.resources.model.ConditionHelper;
import net.minecraft.client.renderer.block.model.BlockModelDefinition;
import net.minecraft.client.renderer.block.model.multipart.Condition;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.properties.WallSide;

import java.util.Objects;
import java.util.Optional;
import java.util.function.UnaryOperator;

public final class WallMultiPartTranslator extends MultiPartTranslator {

    public WallMultiPartTranslator() {
        super(DiagonalBlockTypes.WALL);
    }

    @Override
    protected Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
        if (newProperty.getValueClass() == WallSide.class) {
            return Objects.equals(oldValue, Boolean.TRUE) ? WallSide.LOW : WallSide.NONE;
        } else {
            return super.getNewPropertyValue(oldProperty, newProperty, oldValue);
        }
    }

    @Override
    protected BlockModelDefinition.MultiPartDefinition getModelFromBase(BlockModelDefinition.MultiPartDefinition multiPart) {
        ImmutableList.Builder<Selector> builder = ImmutableList.builder();
        for (Selector selector : multiPart.selectors()) {
            if (selector.condition().isEmpty()) {
                builder.add(selector);
            } else {
                Condition condition = ConditionHelper.deepCopy(selector.condition().get(),
                        UnaryOperator.identity(),
                        (String key, String value) -> {
                            if (value.equals(WallSide.LOW.toString())) {
                                return Boolean.TRUE.toString();
                            } else if (value.equals(WallSide.NONE.toString())) {
                                return Boolean.FALSE.toString();
                            } else if (value.equals(WallSide.TALL.toString())) {
                                return null;
                            } else {
                                return value;
                            }
                        });
                if (condition != null) {
                    builder.add(new Selector(Optional.of(condition), selector.variant()));
                }
            }
        }
        return new BlockModelDefinition.MultiPartDefinition(builder.build());
    }

    @Override
    public boolean allowBaseModelAsFallback() {
        // since we change the block states on our wall block implementation from vanilla,
        // it is not safe to use the model of the vanilla wall base block
        return false;
    }
}
