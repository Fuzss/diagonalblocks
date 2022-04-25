package fuzs.diagonalfences.client.json.adapter;

import fuzs.diagonalfences.client.util.IAdapterUtils;
import com.google.gson.*;
import net.minecraft.client.renderer.block.model.BlockElementFace;

import java.lang.reflect.Type;
import java.util.Map;

/**
 * adapted from {@link net.minecraft.client.renderer.block.model.BlockElementFace.Deserializer}
 */
public class BlockElementFaceAdapter extends BlockElementFace.Deserializer implements JsonSerializer<BlockElementFace>, IAdapterUtils {

    @Override
    public JsonElement serialize(BlockElementFace src, Type typeOfSrc, JsonSerializationContext context) {

        JsonObject blockElementFace = new JsonObject();
        blockElementFace.addProperty("texture", this.serializeLocOrKey(src.texture));
        JsonElement blockFaceUV = context.serialize(src.uv);
        for (Map.Entry<String, JsonElement> entry : blockFaceUV.getAsJsonObject().entrySet()) {

            blockElementFace.add(entry.getKey(), entry.getValue());
        }

        if (src.cullForDirection != null) {

            blockElementFace.addProperty("cullface", src.cullForDirection.getSerializedName());
        }


        if (src.tintIndex != -1) {

            blockElementFace.addProperty("tintindex", src.tintIndex);
        }
        
        return blockElementFace;
    }

}
