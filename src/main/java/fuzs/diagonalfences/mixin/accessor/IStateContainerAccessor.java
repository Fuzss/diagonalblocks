package fuzs.diagonalfences.mixin.accessor;

import com.mojang.serialization.MapCodec;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.StateHolder;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Invoker;

import java.util.function.Supplier;

@SuppressWarnings("unused")
@Mixin(StateDefinition.class)
public interface IStateContainerAccessor<O, S extends StateHolder<O, S>> {

    @Invoker("appendPropertyCodec")
    static <S extends StateHolder<?, S>, T extends Comparable<T>> MapCodec<S> callSetPropertyCodec(MapCodec<S> p_241487_0_, Supplier<S> p_241487_1_, String p_241487_2_, Property<T> p_241487_3_) {

        throw new IllegalStateException();
    }

}
