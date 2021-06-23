package fuzs.diagonalfences.resources;

import fuzs.puzzleslib.json.JsonConfigFileUtil;
import com.google.gson.JsonElement;
import net.minecraft.util.ResourceLocation;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Map;

@SuppressWarnings("unused")
public interface IResourceGenerator {

    boolean hasResourceLocation(ResourceLocation resource);

    Collection<ResourceLocation> getResourceLocations();

    Map<ResourceLocation, byte[]> getResource(ResourceLocation resource);

    Map<ResourceLocation, byte[]> getResources();

    static byte[] toBytes(JsonElement jsonElement) {

        return JsonConfigFileUtil.GSON.toJson(jsonElement).getBytes(StandardCharsets.UTF_8);
    }

}
