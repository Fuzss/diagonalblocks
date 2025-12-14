package fuzs.diagonalblocks.neoforge.impl.client.extensions;

import fuzs.diagonalblocks.impl.client.util.DestroyEffectsHelper;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.extensions.common.IClientBlockExtensions;

public final class DiagonalClientBlockExtensions implements IClientBlockExtensions {

    @Override
    public boolean addDestroyEffects(BlockState blockState, Level level, BlockPos blockPos, ParticleEngine particleEngine) {
        return DestroyEffectsHelper.addDestroyEffects(blockState, level, blockPos, particleEngine) ||
                IClientBlockExtensions.super.addDestroyEffects(blockState, level, blockPos, particleEngine);
    }
}
