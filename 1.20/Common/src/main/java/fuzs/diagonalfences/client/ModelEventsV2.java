package fuzs.diagonalfences.client;

import fuzs.puzzleslib.api.event.v1.core.EventInvoker;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelBaker;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;

import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

public final class ModelEventsV2 {
    public static final EventInvoker<ModifyUnbakedModel> MODIFY_UNBAKED_MODEL = EventInvoker.lookup(ModifyUnbakedModel.class);
    public static final EventInvoker<ModifyBakedModel> MODIFY_BAKED_MODEL = EventInvoker.lookup(ModifyBakedModel.class);

    private ModelEventsV2() {

    }

    @FunctionalInterface
    public interface ModifyUnbakedModel {

        EventResultHolder<UnbakedModel> onModifyUnbakedModel(ResourceLocation modelLocation, UnbakedModel unbakedModel, Function<ResourceLocation, UnbakedModel> modelGetter, BiConsumer<ResourceLocation, UnbakedModel> modelAdder);
    }

    @FunctionalInterface
    public interface ModifyBakedModel {

        EventResultHolder<BakedModel> onModifyBakedModel(ResourceLocation modelLocation, BakedModel bakedModel, Supplier<ModelBaker> modelBaker, Function<ResourceLocation, BakedModel> modelGetter, BiConsumer<ResourceLocation, BakedModel> modelAdder);
    }
}
