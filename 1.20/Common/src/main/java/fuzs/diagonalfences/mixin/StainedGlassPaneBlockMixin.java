package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.world.level.block.StarCollisionBlock;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.StainedGlassPaneBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(StainedGlassPaneBlock.class)
abstract class StainedGlassPaneBlockMixin extends IronBarsBlock implements StarCollisionBlock {

    public StainedGlassPaneBlockMixin(Properties properties) {
        super(properties);
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    public void init(DyeColor dyeColor, BlockBehaviour.Properties properties, CallbackInfo callback) {
        if (this.hasProperties()) {
            this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
        }
    }
}
