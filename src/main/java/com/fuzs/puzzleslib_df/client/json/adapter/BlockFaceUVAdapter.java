package com.fuzs.puzzleslib_df.client.json.adapter;

import com.fuzs.puzzleslib_df.client.util.IAdapterUtils;
import com.google.gson.*;
import net.minecraft.client.renderer.model.BlockFaceUV;

import java.lang.reflect.Type;

/**
 * adapted from {@link net.minecraft.client.renderer.model.BlockFaceUV.Deserializer}
 */
public class BlockFaceUVAdapter extends BlockFaceUV.Deserializer implements JsonSerializer<BlockFaceUV>, IAdapterUtils {

    @Override
    public JsonElement serialize(BlockFaceUV src, Type typeOfSrc, JsonSerializationContext context) {

        JsonObject jsonObject = new JsonObject();
        this.addUVs(jsonObject, src.uvs);
        if (src.rotation != 0) {

            this.addRotation(jsonObject, src.rotation);
        }
        
        return jsonObject;
    }

    private void addUVs(JsonObject jsonObject, float[] uvs) {
        
        if (uvs.length == 4) {

            jsonObject.add("uv", this.serializeFloatArray(uvs));
        } else {

            throw new JsonParseException("Expected 4 uv values, found: " + uvs.length);
        }
    }

    private void addRotation(JsonObject jsonObject, int rotation) {

        if (rotation >= 0 && rotation % 90 == 0 && rotation / 90 <= 3) {

            jsonObject.addProperty("rotation", rotation);
        } else {

            throw new JsonParseException("Invalid rotation " + rotation + " found, only 0/90/180/270 allowed");
        }
    }

}
