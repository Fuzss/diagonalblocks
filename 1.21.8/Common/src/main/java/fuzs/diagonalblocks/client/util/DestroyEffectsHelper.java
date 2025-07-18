package fuzs.diagonalblocks.client.util;

import fuzs.diagonalblocks.world.phys.shapes.VoxelCollection;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.client.particle.TerrainParticle;
import net.minecraft.core.BlockPos;
import net.minecraft.util.Mth;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

public final class DestroyEffectsHelper {

    private DestroyEffectsHelper() {
        // NO-OP
    }

    public static boolean addDestroyEffects(BlockState blockState, Level level, BlockPos blockPos, ParticleEngine particleEngine) {
        if (blockState.getShape(level, blockPos) instanceof VoxelCollection voxelCollection) {
            voxelCollection.forAllParticleBoxes((double x1, double y1, double z1, double x2, double y2, double z2) -> {
                double d1 = Math.min(1.0D, x2 - x1);
                double d2 = Math.min(1.0D, y2 - y1);
                double d3 = Math.min(1.0D, z2 - z1);
                int i = Math.max(2, Mth.ceil(d1 / 0.25D));
                int j = Math.max(2, Mth.ceil(d2 / 0.25D));
                int k = Math.max(2, Mth.ceil(d3 / 0.25D));

                for (int l = 0; l < i; ++l) {
                    for (int i1 = 0; i1 < j; ++i1) {
                        for (int j1 = 0; j1 < k; ++j1) {
                            double d4 = ((double) l + 0.5D) / (double) i;
                            double d5 = ((double) i1 + 0.5D) / (double) j;
                            double d6 = ((double) j1 + 0.5D) / (double) k;
                            double d7 = d4 * d1 + x1;
                            double d8 = d5 * d2 + y1;
                            double d9 = d6 * d3 + z1;
                            particleEngine.add(new TerrainParticle((ClientLevel) level,
                                    (double) blockPos.getX() + d7,
                                    (double) blockPos.getY() + d8,
                                    (double) blockPos.getZ() + d9,
                                    d4 - 0.5D,
                                    d5 - 0.5D,
                                    d6 - 0.5D,
                                    blockState,
                                    blockPos));
                        }
                    }
                }
            });

            return true;
        } else {
            return false;
        }
    }
}
