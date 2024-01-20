package fuzs.diagonalblocks.client.extensions;

import fuzs.diagonalblocks.client.util.DestroyEffectsHelper;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.extensions.common.IClientBlockExtensions;

public final class DiagonalClientBlockExtensions implements IClientBlockExtensions {

    @Override
    public boolean addDestroyEffects(BlockState state, Level level, BlockPos pos, ParticleEngine manager) {
        return DestroyEffectsHelper.addDestroyEffects(state, level, pos, manager) || IClientBlockExtensions.super.addDestroyEffects(state, level, pos, manager);
    }
}
