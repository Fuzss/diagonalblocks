package fuzs.diagonalfences.client.json.adapter;

import fuzs.diagonalfences.client.util.IAdapterUtils;
import com.google.gson.*;
import net.minecraft.client.renderer.model.*;
import net.minecraft.util.Direction;

import java.lang.reflect.Type;
import java.util.Map;

public class BlockPartAdapter extends BlockPart.Deserializer implements JsonSerializer<BlockPart>, IAdapterUtils {

    public static final Gson GSON = new GsonBuilder()
            .registerTypeAdapter(BlockPart.class, new BlockPartAdapter())
            .registerTypeAdapter(BlockPartFace.class, new BlockPartFaceAdapter())
            .registerTypeAdapter(BlockPartRotation.class, new BlockPartRotationAdapter())
            .registerTypeAdapter(BlockFaceUV.class, new BlockFaceUVAdapter())
            .create();
    
    @Override
    public JsonElement serialize(BlockPart src, Type typeOfSrc, JsonSerializationContext context) {

        JsonObject blockPart = new JsonObject();
        blockPart.add("from", this.serializeVector3f(src.positionFrom));
        blockPart.add("to", this.serializeVector3f(src.positionTo));
        if (src.partRotation != null) {

            blockPart.add("rotation", context.serialize(src.partRotation));
        }

        if (!src.shade) {

            blockPart.addProperty("shade", false);
        }

        this.addFaces(context, blockPart, src.mapFaces);

        return blockPart;
    }

    private void addFaces(JsonSerializationContext context, JsonObject jsonObject, Map<Direction, BlockPartFace> mapFaces) {

        JsonObject faces = new JsonObject();
        for (Map.Entry<Direction, BlockPartFace> entry : mapFaces.entrySet()) {

            if (entry.getValue() != null) {

                faces.add(entry.getKey().getName2(), context.serialize(entry.getValue()));
            }
        }

        if (faces.size() != 0) {

            jsonObject.add("faces", faces);
        }
    }

}
