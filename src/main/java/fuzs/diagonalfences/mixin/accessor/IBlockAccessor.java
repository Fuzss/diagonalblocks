package fuzs.diagonalfences.mixin.accessor;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.state.StateContainer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(Block.class)
public interface IBlockAccessor {

    @Accessor
    void setStateContainer(StateContainer<Block, BlockState> stateContainer);

}
