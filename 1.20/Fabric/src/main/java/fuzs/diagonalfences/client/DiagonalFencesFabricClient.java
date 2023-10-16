package fuzs.diagonalfences.client;

import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.model.MultipartAppender;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.model.loading.v1.BlockStateResolver;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelLoadingPlugin;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelModifier;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelResolver;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class DiagonalFencesFabricClient implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        ModelLoadingPlugin.register(pluginContext -> {
            Map<ResourceLocation, UnbakedModel> models = Maps.newHashMap();
            Map<? extends ResourceLocation, BlockState> blocks = BuiltInRegistries.BLOCK.stream()
                    .filter(block -> (block instanceof FenceBlock || block instanceof IronBarsBlock) && block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties())
                    .map(block -> block.getStateDefinition().any())
                    .collect(Collectors.toUnmodifiableMap(BlockModelShaper::stateToModelLocation, Function.identity()));
            pluginContext.modifyModelOnLoad().register((UnbakedModel model, ModelModifier.OnLoad.Context context) -> {
                if (blocks.containsKey(context.id())) {
                    if (model instanceof MultiPart multiPart) {
                        MultipartAppender.appendDiagonalSelectors(models::put, multiPart, blocks.get(context.id()).getBlock() instanceof IronBarsBlock);
                    } else {
                        DiagonalFences.LOGGER.warn("Block '{}' is not using multipart models, diagonal connections will not be visible!", context.id());
                    }
                }
                return model;
            });
//            pluginContext.modifyModelBeforeBake().register((UnbakedModel model, ModelModifier.BeforeBake.Context context) -> {
//                if (blocks.containsKey(context.id())) {
//                    if (model instanceof MultiPart multiPart) {
//                        MultipartAppender.appendDiagonalSelectors(models::put, multiPart, blocks.get(context.id()).getBlock() instanceof IronBarsBlock);
//                    } else {
//                        DiagonalFences.LOGGER.warn("Block '{}' is not using multipart models, diagonal connections will not be visible!", context.id());
//                    }
//                }
//                return model;
//            });
            pluginContext.resolveModel().register((ModelResolver.Context context) -> {
                return models.get(context.id());
            });
        });
    }
}
