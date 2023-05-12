package fuzs.diagonalfences.client.extensions;

import fuzs.diagonalfences.client.model.DestroyEffectsHelper;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.IBlockRenderProperties;

public final class FenceBlockExtensions implements IBlockRenderProperties {

    @Override
    public boolean addDestroyEffects(BlockState state, Level level, BlockPos pos, ParticleEngine manager) {
        return DestroyEffectsHelper.addDestroyEffects(state, level, pos, manager, () -> {
            return IBlockRenderProperties.super.addDestroyEffects(state, level, pos, manager);
        });
    }
}
