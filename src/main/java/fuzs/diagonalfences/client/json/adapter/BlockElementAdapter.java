package fuzs.diagonalfences.client.json.adapter;

import fuzs.diagonalfences.client.util.IAdapterUtils;
import com.google.gson.*;
import net.minecraft.client.renderer.block.model.BlockElement;
import net.minecraft.client.renderer.block.model.BlockElementFace;
import net.minecraft.client.renderer.block.model.BlockElementRotation;
import net.minecraft.client.renderer.block.model.BlockFaceUV;
import net.minecraft.core.Direction;

import java.lang.reflect.Type;
import java.util.Map;

public class BlockElementAdapter extends BlockElement.Deserializer implements JsonSerializer<BlockElement>, IAdapterUtils {

    public static final Gson GSON = new GsonBuilder()
            .registerTypeAdapter(BlockElement.class, new BlockElementAdapter())
            .registerTypeAdapter(BlockElementFace.class, new BlockElementFaceAdapter())
            .registerTypeAdapter(BlockElementRotation.class, new BlockElementRotationAdapter())
            .registerTypeAdapter(BlockFaceUV.class, new BlockFaceUVAdapter())
            .create();
    
    @Override
    public JsonElement serialize(BlockElement src, Type typeOfSrc, JsonSerializationContext context) {

        JsonObject blockElement = new JsonObject();
        blockElement.add("from", this.serializeVector3f(src.from));
        blockElement.add("to", this.serializeVector3f(src.to));
        if (src.rotation != null) {

            blockElement.add("rotation", context.serialize(src.rotation));
        }

        if (!src.shade) {

            blockElement.addProperty("shade", false);
        }

        this.addFaces(context, blockElement, src.faces);

        return blockElement;
    }

    private void addFaces(JsonSerializationContext context, JsonObject jsonObject, Map<Direction, BlockElementFace> mapFaces) {

        JsonObject faces = new JsonObject();
        for (Map.Entry<Direction, BlockElementFace> entry : mapFaces.entrySet()) {

            if (entry.getValue() != null) {

                faces.add(entry.getKey().getName(), context.serialize(entry.getValue()));
            }
        }

        if (faces.size() != 0) {

            jsonObject.add("faces", faces);
        }
    }

}
