package fuzs.diagonalfences.state;

import com.google.common.collect.Maps;
import net.minecraft.state.Property;
import net.minecraft.state.StateContainer;
import net.minecraft.state.StateHolder;

import java.util.Map;
import java.util.function.Function;

public class ExposedStateContainerBuilder<O, S extends StateHolder<O, S>> extends StateContainer.Builder<O, S> {

    public ExposedStateContainerBuilder() {

        super(null);
    }

    public final Map<String, Property<?>> properties = Maps.newHashMap();

    @Override
    public StateContainer.Builder<O, S> add(Property<?>... propertiesIn) {

        for(Property<?> property : propertiesIn) {

            this.properties.put(property.getName(), property);
        }

        return this;
    }

    @Override
    public StateContainer<O, S> func_235882_a_(Function<O, S> p_235882_1_, StateContainer.IFactory<O, S> p_235882_2_) {

        // this builder is only meant to expose properties map
        throw new UnsupportedOperationException();
    }

}
