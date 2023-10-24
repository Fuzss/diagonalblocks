package fuzs.diagonalfences.mixin.client;

import fuzs.diagonalfences.client.util.DestroyEffectsHelper;
import fuzs.diagonalfences.world.level.block.StarShapeProvider;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.state.BlockState;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ParticleEngine.class)
abstract class ParticleEngineFabricMixin {
    @Shadow
    protected ClientLevel level;

    @Inject(method = "destroy", at = @At("HEAD"), cancellable = true)
    public void destroy(BlockPos blockPos, BlockState blockState, CallbackInfo callback) {
        if (blockState.getBlock() instanceof StarShapeProvider) {
            if (DestroyEffectsHelper.addDestroyEffects(blockState, this.level, blockPos, ParticleEngine.class.cast(this))) {
                callback.cancel();
            }
        }
    }
}
