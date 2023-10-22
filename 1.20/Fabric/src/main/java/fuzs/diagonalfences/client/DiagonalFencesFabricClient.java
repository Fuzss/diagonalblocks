package fuzs.diagonalfences.client;

import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.mixin.client.ModelBakeryFabricAccessor;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import fuzs.puzzleslib.api.event.v1.core.FabricEventInvokerRegistry;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelLoadingPlugin;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelModifier;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelResolver;
import net.minecraft.client.resources.model.*;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.Nullable;

import java.util.Map;

public class DiagonalFencesFabricClient implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        registerEventInvokers();
        ClientModConstructor.construct(DiagonalFences.MOD_ID, DiagonalFencesClient::new);
    }

    private static void registerEventInvokers() {
        FabricEventInvokerRegistry.INSTANCE.register(ModelEvents.ModifyUnbakedModel.class, (ModelEvents.ModifyUnbakedModel callback, @Nullable Object o) -> {
            ModelLoadingPlugin.register(pluginContext -> {
                Map<ResourceLocation, UnbakedModel> additionalUnbakedModels = Maps.newHashMap();
                Map<UnbakedModel, UnbakedModel> modelCache = Maps.newIdentityHashMap();
                pluginContext.modifyModelBeforeBake().register(ModelModifier.OVERRIDE_PHASE, (UnbakedModel model, ModelModifier.BeforeBake.Context context) -> {
                    if (!((ModelBakeryFabricAccessor) context.loader()).puzzleslib$getTopLevelModels().containsKey(context.id())) return model;
                    EventResultHolder<UnbakedModel> result = callback.onModifyUnbakedModel(context.id(), model, context.loader()::getModel, additionalUnbakedModels::put, modelCache.get(model));
                    result.getInterrupt().ifPresent(unbakedModel -> modelCache.put(model, unbakedModel));
                    return modelCache.getOrDefault(model, model);
                });
                pluginContext.resolveModel().register((ModelResolver.Context context) -> {
                    return additionalUnbakedModels.get(context.id());
                });
            });
        });
        FabricEventInvokerRegistry.INSTANCE.register(ModelEvents.ModifyBakedModel.class, (ModelEvents.ModifyBakedModel callback, @Nullable Object o) -> {
            ModelLoadingPlugin.register(pluginContext -> {
                pluginContext.modifyModelAfterBake().register(ModelModifier.OVERRIDE_PHASE, (@Nullable BakedModel model, ModelModifier.AfterBake.Context context) -> {
                    if (model != null) {
                        Map<ResourceLocation, BakedModel> models = context.loader().getBakedTopLevelModels();
                        EventResultHolder<BakedModel> result = callback.onModifyBakedModel(context.id(), model, context::baker, (ResourceLocation resourceLocation) -> {
                            return models.containsKey(resourceLocation) ? models.get(resourceLocation) : context.baker().bake(resourceLocation, BlockModelRotation.X0_Y0);
                        }, models::putIfAbsent);
                        return result.getInterrupt().orElse(model);
                    } else {
                        return null;
                    }
                });
            });
        });
        FabricEventInvokerRegistry.INSTANCE.register(ModelEvents.AdditionalBakedModel.class, (ModelEvents.AdditionalBakedModel callback, @Nullable Object o) -> {
            ModelLoadingPlugin.register(pluginContext -> {
                pluginContext.modifyModelAfterBake().register((@Nullable BakedModel model, ModelModifier.AfterBake.Context context) -> {
                    // all we want is access to the top level baked models map to be able to insert our own models
                    // since the missing model is guaranteed to be baked at some point hijack the event to get to the map
                    if (context.id().equals(ModelBakery.MISSING_MODEL_LOCATION)) {
                        Map<ResourceLocation, BakedModel> models = context.loader().getBakedTopLevelModels();
                        // using the baker from the context will print the wrong model for missing textures (missing model), but that's how it is
                        callback.onAdditionalBakedModel(models::putIfAbsent, (ResourceLocation resourceLocation) -> {
                            return models.containsKey(resourceLocation) ? models.get(resourceLocation) : context.baker().bake(resourceLocation, BlockModelRotation.X0_Y0);
                        }, context::baker);
                    }
                    return model;
                });
            });
        });
    }
}
