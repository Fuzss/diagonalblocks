package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.util.DestroyEffectsUtil;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.extensions.common.IClientBlockExtensions;
import org.spongepowered.asm.mixin.Mixin;

import java.util.function.Consumer;

@Mixin(FenceBlock.class)
abstract class FenceBlockForgeMixin extends CrossCollisionBlock {

    public FenceBlockForgeMixin(float pNodeWidth, float pExtensionWidth, float pNodeHeight, float pExtensionHeight, float pCollisionHeight, Properties pProperties) {
        super(pNodeWidth, pExtensionWidth, pNodeHeight, pExtensionHeight, pCollisionHeight, pProperties);
    }

    @Override
    public void initializeClient(Consumer<IClientBlockExtensions> consumer) {
        consumer.accept(new IClientBlockExtensions() {

            @Override
            public boolean addDestroyEffects(BlockState state, Level level, BlockPos pos, ParticleEngine manager) {
                return DestroyEffectsUtil.addDestroyEffects(state, level, pos, manager) || IClientBlockExtensions.super.addDestroyEffects(state, level, pos, manager);
            }
        });
    }
}
