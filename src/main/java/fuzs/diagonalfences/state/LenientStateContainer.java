package fuzs.diagonalfences.state;

import com.google.common.collect.*;
import com.mojang.datafixers.util.Pair;
import com.mojang.serialization.Decoder;
import com.mojang.serialization.Encoder;
import com.mojang.serialization.MapCodec;
import fuzs.diagonalfences.mixin.accessor.IStateContainerAccessor;
import net.minecraft.state.Property;
import net.minecraft.state.StateContainer;
import net.minecraft.state.StateHolder;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class LenientStateContainer<O, S extends StateHolder<O, S>> extends StateContainer<O, S> {

    @SuppressWarnings({"unchecked", "UnstableApiUsage"})
    protected LenientStateContainer(Function<O, S> defaultState, O owner, StateContainer.IFactory<O, S> defaultFactory, Map<String, Property<?>> allProperties, Set<String> additionalProperties) {

        super(defaultState, owner, defaultFactory, allProperties);
        Supplier<S> supplier = () -> defaultState.apply(owner);
        MapCodec<S> mapcodec = MapCodec.of(Encoder.empty(), Decoder.unit(supplier));

        for (Map.Entry<String, Property<?>> entry : ImmutableSortedMap.copyOf(allProperties).entrySet()) {

            if (!additionalProperties.contains(entry.getKey())) {

                mapcodec = IStateContainerAccessor.callSetPropertyCodec(mapcodec, supplier, entry.getKey(), entry.getValue());
            }
        }

        MapCodec<S> mapcodec1 = mapcodec;
        Map<Map<Property<?>, Comparable<?>>, S> map = Maps.newLinkedHashMap();
        List<S> list = Lists.newArrayList();
        Stream<List<Pair<Property<?>, Comparable<?>>>> stream = Stream.of(Collections.emptyList());

        for (Property<?> property : this.getProperties()) {
            stream = stream.flatMap((p_200999_1_) -> property.getAllowedValues().stream().map((p_200998_2_) -> {
                List<Pair<Property<?>, Comparable<?>>> list1 = Lists.newArrayList(p_200999_1_);
                list1.add(Pair.of(property, p_200998_2_));
                return list1;
            }));
        }

        stream.forEach((p_201000_5_) -> {

            ImmutableMap<Property<?>, Comparable<?>> immutablemap = p_201000_5_.stream().collect(ImmutableMap.toImmutableMap(Pair::getFirst, Pair::getSecond));
            S s1 = defaultFactory.create(owner, immutablemap, mapcodec1);
            map.put(immutablemap, s1);
            list.add(s1);
        });

        for(S s : list) {

            s.func_235899_a_(map);
        }

        ((IStateContainerAccessor<O, S>) this).setValidStates(ImmutableList.copyOf(list));
    }

    public static class LenientBuilder<O, S extends StateHolder<O, S>> extends StateContainer.Builder<O, S> {

        private final O owner;
        private final Map<String, Property<?>> properties = Maps.newHashMap();
        private final Set<String> additionalProperties = Sets.newHashSet();

        public LenientBuilder(O object) {

            super(object);
            this.owner = object;
        }

        @Override
        public LenientBuilder<O, S> add(Property<?>... propertiesIn) {

            for(Property<?> property : propertiesIn) {

                // there is normally a validation happening here, but we don't need that as properties are already validated before this runs by vanilla StateContainer.Builder
                this.properties.put(property.getName(), property);
            }

            return this;
        }

        @SuppressWarnings("UnusedReturnValue")
        public LenientBuilder<O, S> addAdditional(Property<?>... propertiesIn) {

            for (Property<?> property : propertiesIn) {

                // there is normally a validation happening here, but we don't need that as properties are already validated before this runs by vanilla StateContainer.Builder
                this.additionalProperties.add(property.getName());
            }

            return this;
        }

        @Override
        public StateContainer<O, S> func_235882_a_(Function<O, S> p_235882_1_, StateContainer.IFactory<O, S> p_235882_2_) {

            return new LenientStateContainer<>(p_235882_1_, this.owner, p_235882_2_, this.properties, this.additionalProperties);
        }

    }

}
