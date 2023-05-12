package fuzs.diagonalfences.client.model;

import fuzs.diagonalfences.world.phys.shapes.VoxelCollection;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.client.particle.TerrainParticle;
import net.minecraft.core.BlockPos;
import net.minecraft.util.Mth;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.function.BooleanSupplier;

public class DestroyEffectsHelper {
    public static boolean addDestroyEffects(BlockState state, Level level, BlockPos pos, ParticleEngine manager, BooleanSupplier altResult) {
        VoxelShape voxelshape = state.getShape(level, pos);
        if (!(voxelshape instanceof VoxelCollection voxelCollection)) return altResult.getAsBoolean();
        voxelCollection.forAllParticleBoxes((p_172273_, p_172274_, p_172275_, p_172276_, p_172277_, p_172278_) -> {
            double d1 = Math.min(1.0D, p_172276_ - p_172273_);
            double d2 = Math.min(1.0D, p_172277_ - p_172274_);
            double d3 = Math.min(1.0D, p_172278_ - p_172275_);
            int i = Math.max(2, Mth.ceil(d1 / 0.25D));
            int j = Math.max(2, Mth.ceil(d2 / 0.25D));
            int k = Math.max(2, Mth.ceil(d3 / 0.25D));

            for(int l = 0; l < i; ++l) {
                for(int i1 = 0; i1 < j; ++i1) {
                    for(int j1 = 0; j1 < k; ++j1) {
                        double d4 = ((double)l + 0.5D) / (double)i;
                        double d5 = ((double)i1 + 0.5D) / (double)j;
                        double d6 = ((double)j1 + 0.5D) / (double)k;
                        double d7 = d4 * d1 + p_172273_;
                        double d8 = d5 * d2 + p_172274_;
                        double d9 = d6 * d3 + p_172275_;
                        manager.add(new TerrainParticle((ClientLevel) level, (double)pos.getX() + d7, (double)pos.getY() + d8, (double)pos.getZ() + d9, d4 - 0.5D, d5 - 0.5D, d6 - 0.5D, state, pos));
                    }
                }
            }

        });
        return true;
    }
}
