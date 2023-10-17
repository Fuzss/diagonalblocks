package fuzs.diagonalfences.client;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.model.MultipartAppender;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelLoadingPlugin;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelModifier;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelResolver;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class DiagonalFencesFabricClient implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        ModelLoadingPlugin.register(pluginContext -> {
            Map<ResourceLocation, UnbakedModel> additionalUnbakedModels = Maps.newHashMap();
            Map<? extends ResourceLocation, Block> blocks = BuiltInRegistries.BLOCK.stream()
                    .filter(block -> block instanceof FenceBlock || block instanceof IronBarsBlock)
                    .filter(block -> block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties())
                    .flatMap(block -> block.getStateDefinition().getPossibleStates().stream())
                    .collect(Collectors.toUnmodifiableMap(BlockModelShaper::stateToModelLocation, BlockBehaviour.BlockStateBase::getBlock));
            Set<UnbakedModel> unbakedModels = Sets.newIdentityHashSet();
            pluginContext.modifyModelOnLoad().register((UnbakedModel model, ModelModifier.OnLoad.Context context) -> {
                if (blocks.containsKey(context.id()) && !unbakedModels.contains(model)) {
                    Block block = blocks.get(context.id());
                    if (model instanceof MultiPart multiPart) {
                        MultipartAppender.appendDiagonalSelectors(additionalUnbakedModels::put, multiPart, block instanceof IronBarsBlock);
                    } else {
                        DiagonalFences.LOGGER.warn("Block '{}' is not using multipart models, diagonal connections will not be visible!", block);
                    }
                    unbakedModels.add(model);
                }
                return model;
            });
            pluginContext.resolveModel().register((ModelResolver.Context context) -> {
                return additionalUnbakedModels.get(context.id());
            });
        });
    }
}
