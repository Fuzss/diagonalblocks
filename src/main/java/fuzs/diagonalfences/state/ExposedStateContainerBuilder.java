package fuzs.diagonalfences.state;

import com.google.common.collect.Maps;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.StateHolder;

import java.util.Map;
import java.util.function.Function;

public class ExposedStateContainerBuilder<O, S extends StateHolder<O, S>> extends StateDefinition.Builder<O, S> {

    public ExposedStateContainerBuilder() {

        super(null);
    }

    public final Map<String, Property<?>> properties = Maps.newHashMap();

    @Override
    public StateDefinition.Builder<O, S> add(Property<?>... propertiesIn) {

        for(Property<?> property : propertiesIn) {

            this.properties.put(property.getName(), property);
        }

        return this;
    }

    @Override
    public StateDefinition<O, S> create(Function<O, S> p_235882_1_, StateDefinition.Factory<O, S> p_235882_2_) {

        // this builder is only meant to expose properties map
        throw new UnsupportedOperationException();
    }

}
