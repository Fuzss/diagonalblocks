package fuzs.diagonalblocks.fabric.mixin.client;

import fuzs.diagonalblocks.api.v2.block.StarShapeProvider;
import fuzs.diagonalblocks.impl.client.util.DestroyEffectsHelper;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.dimension.DimensionType;
import net.minecraft.world.level.storage.WritableLevelData;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ClientLevel.class)
abstract class ClientLevelFabricMixin extends Level {
    @Shadow
    @Final
    private Minecraft minecraft;

    protected ClientLevelFabricMixin(WritableLevelData levelData, ResourceKey<Level> dimension, RegistryAccess registryAccess, Holder<DimensionType> dimensionTypeRegistration, boolean isClientSide, boolean isDebug, long biomeZoomSeed, int maxChainedNeighborUpdates) {
        super(levelData,
                dimension,
                registryAccess,
                dimensionTypeRegistration,
                isClientSide,
                isDebug,
                biomeZoomSeed,
                maxChainedNeighborUpdates);
    }

    @Inject(method = "addDestroyBlockEffect", at = @At("HEAD"), cancellable = true)
    public void addDestroyBlockEffect(BlockPos blockPos, BlockState blockState, CallbackInfo callback) {
        if (blockState.getBlock() instanceof StarShapeProvider) {
            if (DestroyEffectsHelper.addDestroyEffects(blockState, this, blockPos, this.minecraft.particleEngine)) {
                callback.cancel();
            }
        }
    }
}
