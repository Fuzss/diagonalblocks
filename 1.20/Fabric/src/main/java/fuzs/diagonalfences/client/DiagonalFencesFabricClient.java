package fuzs.diagonalfences.client;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.util.MultipartAppender;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import fuzs.puzzleslib.api.event.v1.core.FabricEventInvokerRegistry;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelLoadingPlugin;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelModifier;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelResolver;
import net.fabricmc.fabric.api.event.Event;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
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
            pluginContext.modifyModelAfterBake().register((@Nullable BakedModel model, ModelModifier.AfterBake.Context context) -> {
                context.
            });
        });
        FabricEventInvokerRegistry.INSTANCE.register(ModelEventsV2.ModifyUnbakedModel.class, ModelModifier.OnLoad.class, callback -> {
            ModelLoadingPlugin.register(pluginContext -> {
                Map<ResourceLocation, UnbakedModel> additionalUnbakedModels = Maps.newHashMap();
                pluginContext.modifyModelOnLoad().register((UnbakedModel model, ModelModifier.OnLoad.Context context) -> {
                    EventResultHolder<UnbakedModel> result = callback.onModifyUnbakedModel(context.id(), model, context::getOrLoadModel, additionalUnbakedModels::put);
                    return result.getInterrupt().orElse(model);
                });
                pluginContext.resolveModel().register((ModelResolver.Context context) -> {
                    return additionalUnbakedModels.get(context.id());
                });
            });
            return (model, context) -> {
//                callback.onModifyUnbakedModel(context.id(), model, context::getOrLoadModel, )
                return model;
            };
        }, (Object context, Consumer<Event<ModelModifier.OnLoad>> applyToInvoker, Consumer<Event<ModelModifier.OnLoad>> removeInvoker) -> {
            ModelLoadingPlugin.register(pluginContext -> {
                applyToInvoker.accept(pluginContext.modifyModelOnLoad());
            });
        });
    }
}
