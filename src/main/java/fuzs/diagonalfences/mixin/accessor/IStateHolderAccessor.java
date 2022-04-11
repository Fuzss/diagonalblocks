package fuzs.diagonalfences.mixin.accessor;

import com.mojang.serialization.MapCodec;
import net.minecraft.world.level.block.state.StateHolder;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Mutable;
import org.spongepowered.asm.mixin.gen.Accessor;

@SuppressWarnings("unused")
@Mixin(StateHolder.class)
public interface IStateHolderAccessor<O, S> {

    @Mutable //Java 9+ checks write access to final fields more strictly
    @Accessor("propertiesCodec")
    void setCodec(MapCodec<S> codec);

}
