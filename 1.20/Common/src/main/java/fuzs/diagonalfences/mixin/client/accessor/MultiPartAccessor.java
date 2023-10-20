package fuzs.diagonalfences.mixin.client.accessor;

import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(MultiPart.class)
public interface MultiPartAccessor {

    @Accessor("definition")
    StateDefinition<Block, BlockState> diagonalfences$getDefinition();
}
