package com.fuzs.diagonalfences.mixin;

import com.fuzs.diagonalfences.block.IEightWayBlock;
import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.state.StateContainer;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@SuppressWarnings("unused")
@Mixin(Block.class)
public abstract class BlockMixin extends AbstractBlock {

    @Shadow
    @Final
    protected StateContainer<Block, BlockState> stateContainer;

    public BlockMixin(Properties properties) {

        super(properties);
    }

    @SuppressWarnings("ConstantConditions")
    @Inject(method = "<init>", at = @At("TAIL"))
    public void init(AbstractBlock.Properties properties, CallbackInfo callbackInfo) {

        // mods might override Block::fillStateContainer so our properties won't be added
        // this leads to a crash on start-up though, constructing everything again seems like a solution to this
        StateContainer.Builder<Block, BlockState> builder = new StateContainer.Builder<>((Block) (Object) this);
        this.fillStateContainer(builder);
        if (this instanceof IEightWayBlock) {

            ((IEightWayBlock) this).fillStateContainer2(builder);
        }

        this.setStateContainer(builder.func_235882_a_(Block::getDefaultState, BlockState::new));
        this.setDefaultState(this.stateContainer.getBaseState());
    }

    @Shadow
    protected abstract void fillStateContainer(StateContainer.Builder<Block, BlockState> builder);

    @Shadow
    protected final void setDefaultState(BlockState state) {

        throw new IllegalStateException();
    }

    @Accessor
    public abstract void setStateContainer(StateContainer<Block, BlockState> stateContainer);

}
