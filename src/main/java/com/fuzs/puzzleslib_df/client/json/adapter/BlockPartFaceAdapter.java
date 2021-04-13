package com.fuzs.puzzleslib_df.client.json.adapter;

import com.fuzs.puzzleslib_df.client.util.IAdapterUtils;
import com.google.gson.*;
import net.minecraft.client.renderer.model.BlockPartFace;

import java.lang.reflect.Type;
import java.util.Map;

/**
 * adapted from {@link net.minecraft.client.renderer.model.BlockPartFace.Deserializer}
 */
public class BlockPartFaceAdapter extends BlockPartFace.Deserializer implements JsonSerializer<BlockPartFace>, IAdapterUtils {

    @Override
    public JsonElement serialize(BlockPartFace src, Type typeOfSrc, JsonSerializationContext context) {

        JsonObject blockPartFace = new JsonObject();
        blockPartFace.addProperty("texture", this.serializeLocOrKey(src.texture));
        JsonElement blockFaceUV = context.serialize(src.blockFaceUV);
        for (Map.Entry<String, JsonElement> entry : blockFaceUV.getAsJsonObject().entrySet()) {

            blockPartFace.add(entry.getKey(), entry.getValue());
        }

        if (src.cullFace != null) {

            blockPartFace.addProperty("cullface", src.cullFace.getString());
        }


        if (src.tintIndex != -1) {

            blockPartFace.addProperty("tintindex", src.tintIndex);
        }
        
        return blockPartFace;
    }

}
