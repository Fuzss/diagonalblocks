package fuzs.diagonalfences.client.util;

import com.google.gson.JsonArray;
import net.minecraft.resources.ResourceLocation;
import com.mojang.math.Vector3f;

public interface IAdapterUtils {

    default String serializeLocOrKey(String tex) {

        return tex.charAt(0) == '#' ? tex : new ResourceLocation(tex).toString();
    }

    default JsonArray serializeFloatArray(float[] array) {

        JsonArray jsonArray = new JsonArray();
        for (float f : array) {

            jsonArray.add(this.serializeFloat(f));
        }

        return jsonArray;
    }

    default JsonArray serializeVector3f(Vector3f vec) {

        JsonArray jsonArray = new JsonArray();
        jsonArray.add(this.serializeFloat(vec.x()));
        jsonArray.add(this.serializeFloat(vec.y()));
        jsonArray.add(this.serializeFloat(vec.z()));

        return jsonArray;
    }

    default Number serializeFloat(float f) {

        if ((int) f == f) {

            return (int) f;
        }

        return f;
    }
    
}
