package fuzs.diagonalblocks.mixin.client;

import com.llamalad7.mixinextras.sugar.Local;
import fuzs.diagonalblocks.client.handler.DiagonalModelHandler;
import net.minecraft.client.resources.model.BlockStateModelLoader;
import net.minecraft.client.resources.model.ModelManager;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.Resource;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.ModifyVariable;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

@Mixin(ModelManager.class)
abstract class ModelManagerMixin {

    @Inject(
            method = "lambda$loadBlockStates$13(Ljava/util/Map$Entry;Ljava/util/function/Function;Lnet/minecraft/client/resources/model/BlockStateModelLoader;)Lnet/minecraft/client/resources/model/BlockStateModelLoader$LoadedModels;",
            at = @At("RETURN")
    )
    private static void loadBlockStates(Map.Entry<ResourceLocation, List<Resource>> entry, Function<ResourceLocation, StateDefinition<Block, BlockState>> stateDefinitionGetter, BlockStateModelLoader blockStateModelLoader, CallbackInfoReturnable<BlockStateModelLoader.LoadedModels> callback, @Local ResourceLocation resourceLocation, @Local(
            ordinal = 1
    ) List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions) {
        // we can no longer use mod loader model events dealing with unbaked models,
        // as we need access to MultiPart$Definition which is the last instance containing uncompiled model selectors
        // (before 1.21.3 these would still be present in MultiPart)
        BlockStateModelLoader.LoadedModels loadedModels = callback.getReturnValue();
        // there are two returns, one is for when loading the model fails and returns null, we do not want that
        if (loadedModels != null) {
            for (BlockStateModelLoader.LoadedBlockModelDefinition loadedBlockModelDefinition : loadedBlockModelDefinitions) {
                BlockStateModelLoader.LoadedModels tmpLoadedModels = DiagonalModelHandler.loadBlockStateDefinition(
                        blockStateModelLoader, resourceLocation, stateDefinitionGetter,
                        loadedBlockModelDefinition.contents()
                );
                if (tmpLoadedModels != null) {
                    // just add the models to this map, the only thing that happens after that is all models being collected once more into a single map
                    loadedModels.models().putAll(tmpLoadedModels.models());
                }
            }
        }
    }

    @ModifyVariable(method = "discoverModelDependencies", at = @At("HEAD"))
    private Map<ResourceLocation, UnbakedModel> discoverModelDependencies(Map<ResourceLocation, UnbakedModel> map) {
        return DiagonalModelHandler.putAdditionalBlockModels(map);
    }
}
