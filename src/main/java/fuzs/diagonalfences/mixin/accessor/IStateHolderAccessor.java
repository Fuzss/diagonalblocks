package fuzs.diagonalfences.mixin.accessor;

import com.mojang.serialization.MapCodec;
import net.minecraft.state.StateHolder;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Mutable;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(StateHolder.class)
public interface IStateHolderAccessor<O, S> {

    @Accessor("field_235893_d_")
    @Mutable
    void setCodec(MapCodec<S> codec);

}
