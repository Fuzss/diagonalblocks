package fuzs.diagonalfences.mixin.accessor;

import com.mojang.serialization.MapCodec;
import net.minecraft.state.Property;
import net.minecraft.state.StateContainer;
import net.minecraft.state.StateHolder;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Invoker;

import java.util.function.Supplier;

@SuppressWarnings("unused")
@Mixin(StateContainer.class)
public interface IStateContainerAccessor<O, S extends StateHolder<O, S>> {

    @Invoker("func_241487_a_")
    static <S extends StateHolder<?, S>, T extends Comparable<T>> MapCodec<S> callSetPropertyCodec(MapCodec<S> p_241487_0_, Supplier<S> p_241487_1_, String p_241487_2_, Property<T> p_241487_3_) {

        throw new IllegalStateException();
    }

}
