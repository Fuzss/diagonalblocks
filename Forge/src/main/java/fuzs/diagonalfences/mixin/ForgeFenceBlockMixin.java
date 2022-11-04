package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.world.phys.shapes.VoxelCollection;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.client.particle.TerrainParticle;
import net.minecraft.core.BlockPos;
import net.minecraft.util.Mth;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.client.IBlockRenderProperties;
import org.spongepowered.asm.mixin.Mixin;

import java.util.function.Consumer;

@Mixin(FenceBlock.class)
public abstract class ForgeFenceBlockMixin extends CrossCollisionBlock {
    
    public ForgeFenceBlockMixin(float p_52320_, float p_52321_, float p_52322_, float p_52323_, float p_52324_, Properties p_52325_) {
        super(p_52320_, p_52321_, p_52322_, p_52323_, p_52324_, p_52325_);
    }

    @Override
    public void initializeClient(Consumer<IBlockRenderProperties> consumer) {
        consumer.accept(new IBlockRenderProperties() {

            @Override
            public boolean addDestroyEffects(BlockState state, Level level, BlockPos pos, ParticleEngine manager) {
                VoxelShape voxelshape = state.getShape(level, pos);
                if (!(voxelshape instanceof VoxelCollection voxelCollection)) return IBlockRenderProperties.super.addDestroyEffects(state, level, pos, manager);
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
                                manager.add(new TerrainParticle((ClientLevel) level, (double)pos.getX() + d7, (double)pos.getY() + d8, (double)pos.getZ() + d9, d4 - 0.5D, d5 - 0.5D, d6 - 0.5D, state, pos).updateSprite(state, pos));
                            }
                        }
                    }

                });
                return true;
            }
        });
    }
}
