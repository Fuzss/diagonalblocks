package fuzs.diagonalfences.client.json.adapter;

import fuzs.diagonalfences.client.util.IAdapterUtils;
import com.google.gson.*;
import net.minecraft.client.renderer.block.model.BlockElementRotation;
import net.minecraft.util.Mth;

import java.lang.reflect.Type;

/**
 * adapted from {@link net.minecraft.client.renderer.block.model.BlockElement.Deserializer}
 */
public class BlockElementRotationAdapter implements JsonSerializer<BlockElementRotation>, IAdapterUtils {

    @Override
    public JsonElement serialize(BlockElementRotation src, Type typeOfSrc, JsonSerializationContext context) {

        JsonObject jsonObject = new JsonObject();
        jsonObject.add("origin", this.serializeVector3f(src.origin));
        jsonObject.addProperty("axis", src.axis.getName());
        this.addAngle(jsonObject, src.angle);
        if (src.rescale) {

            jsonObject.addProperty("rescale", true);
        }

        return jsonObject;
    }

    private void addAngle(JsonObject jsonObject, float angle) {

        if (angle == 0.0F || Mth.abs(angle) == 22.5F || Mth.abs(angle) == 45.0F) {

            jsonObject.addProperty("angle", angle);
        } else {

            throw new JsonParseException("Invalid rotation " + angle + " found, only -45/-22.5/0/22.5/45 allowed");
        }
    }

}
