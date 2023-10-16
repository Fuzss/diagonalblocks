package fuzs.diagonalfences.client;

import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;

public final class ModelEventsV2 {

    private ModelEventsV2() {

    }

    public interface ModifyUnbakedModel {

        EventResultHolder<UnbakedModel> onModifyUnbakedModel(ResourceLocation modelLocation);
    }

    public interface ModifyBakedModel {

        EventResultHolder<BakedModel> onModifyBakedModel(ResourceLocation modelLocation);
    }
}
