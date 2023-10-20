package fuzs.diagonalfences.client;

import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import fuzs.puzzleslib.api.event.v1.core.FabricEventInvokerRegistry;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelLoadingPlugin;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelModifier;
import net.fabricmc.fabric.api.client.model.loading.v1.ModelResolver;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.BlockModelRotation;
import net.minecraft.client.resources.model.UnbakedModel;
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
        FabricEventInvokerRegistry.INSTANCE.register(ModelEventsV2.ModifyUnbakedModel.class, (ModelEventsV2.ModifyUnbakedModel callback, @Nullable Object o) -> {
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
        });
        FabricEventInvokerRegistry.INSTANCE.register(ModelEventsV2.ModifyBakedModel.class, (ModelEventsV2.ModifyBakedModel callback, @Nullable Object o) -> {
            ModelLoadingPlugin.register(pluginContext -> {
                pluginContext.modifyModelAfterBake().register((@Nullable BakedModel model, ModelModifier.AfterBake.Context context) -> {
                    if (model != null) {
                        Map<ResourceLocation, BakedModel> models = context.loader().getBakedTopLevelModels();
                        EventResultHolder<BakedModel> result = callback.onModifyBakedModel(context.id(), model, context::baker, key -> {
                            return models.containsKey(key) ? models.get(key) : context.baker().bake(key, BlockModelRotation.X0_Y0);
                        }, models::put);
                        return result.getInterrupt().orElse(model);
                    } else {
                        return null;
                    }
                });
            });
        });
    }
}
