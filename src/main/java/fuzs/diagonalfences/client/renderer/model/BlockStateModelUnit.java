package fuzs.diagonalfences.client.renderer.model;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.client.json.adapter.BlockElementAdapter;
import fuzs.diagonalfences.client.util.AssetLocations;
import fuzs.puzzleslib.json.JsonConfigFileUtil;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.world.level.block.Block;
import net.minecraft.client.renderer.block.model.BlockModel;
import net.minecraft.client.renderer.block.model.BlockElement;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.resources.ResourceLocation;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

public class BlockStateModelUnit {

    private final ResourceManager resourceManager;
    public final Block block;
    @Nonnull
    public final ResourceLocation blockLocation;
    private final Map<ResourceLocation, JsonElement> resources = Maps.newHashMap();
    private final Map<Pair<String, String>, String> propertyConverter;
    private final Consumer<List<BlockElement>> elementsConverter;

    @SuppressWarnings("ConstantConditions")
    public BlockStateModelUnit(ResourceManager resourceManager, Block block, Map<Pair<String, String>, String> propertyConverter, Consumer<List<BlockElement>> elementsConverter) {

        this.resourceManager = resourceManager;
        this.block = block;
        this.blockLocation = block.getRegistryName();
        this.propertyConverter = propertyConverter;
        // this should really convert the whole model and not just its elements,
        // but that would require serializing the whole model also which there is no need to currently
        this.elementsConverter = elementsConverter;
    }

    public Map<ResourceLocation, JsonElement> load() {

        this.resources.clear();
        JsonElement blockStatesElement = this.loadResource(AssetLocations.getBlockStatesPath(this.blockLocation), reader -> JsonConfigFileUtil.GSON.fromJson(reader, JsonElement.class));
        if (blockStatesElement instanceof JsonObject && blockStatesElement.getAsJsonObject().has("multipart")) {

            // will be added after iterating
            List<JsonElement> newSelectors = Lists.newArrayList();
            JsonArray selectorArray = blockStatesElement.getAsJsonObject().getAsJsonArray("multipart");
            for (JsonElement selectorElement : selectorArray) {

                JsonObject selectorObject = selectorElement.getAsJsonObject();
                if (selectorObject.has("when")) {

                    JsonObject conditionObject = selectorObject.getAsJsonObject("when");
                    // must always be present
                    JsonElement variantListElement = selectorObject.get("apply");
                    this.makeNewSelector(conditionObject, variantListElement).ifPresent(newSelectors::add);
                }
            }

            newSelectors.forEach(selectorArray::add);
            this.resources.put(AssetLocations.getBlockStatesPath(this.blockLocation), blockStatesElement);
        }

        return ImmutableMap.copyOf(this.resources);
    }

    private Optional<JsonElement> makeNewSelector(JsonObject conditionObject, JsonElement variantListElement) {

        // only convert when there is at least one key which would change
        boolean isConvertible = conditionObject.entrySet().stream()
                .map(entry -> Pair.of(entry.getKey(), entry.getValue().getAsString()))
                .anyMatch(this.propertyConverter::containsKey);

        if (isConvertible) {

            JsonObject newSelectorObject = new JsonObject();
            JsonObject newConditionObject = this.getConvertedCondition(conditionObject);
            newSelectorObject.add("when", newConditionObject);
            JsonElement newVariantListElement = this.getConvertedVariantList(variantListElement);
            newSelectorObject.add("apply", newVariantListElement);

            return Optional.of(newSelectorObject);
        }

        return Optional.empty();
    }

    private JsonObject getConvertedCondition(JsonObject whenObject) {

        // convert every property which is convertible and leave the rest as is
        JsonObject convertedWhenObject = new JsonObject();
        for (Map.Entry<String, JsonElement> entry : whenObject.entrySet()) {

            String property = entry.getKey();
            String value = entry.getValue().getAsString();
            property = this.propertyConverter.getOrDefault(Pair.of(property, value), property);
            convertedWhenObject.addProperty(property, value);
        }

        return convertedWhenObject;
    }

    private JsonElement getConvertedVariantList(JsonElement variantElement) {

        if (variantElement.isJsonArray()) {

            JsonArray variantListArray = new JsonArray();
            for (JsonElement variantListElement : variantElement.getAsJsonArray()) {

                variantListArray.add(this.getConvertedVariant(variantListElement));
            }

            return variantListArray;
        }

        return this.getConvertedVariant(variantElement);
    }

    private JsonElement getConvertedVariant(JsonElement variantElement) {

        JsonObject newVariantObject = new JsonObject();
        for (Map.Entry<String, JsonElement> variantEntry : variantElement.getAsJsonObject().entrySet()) {

            String key = variantEntry.getKey();
            JsonElement value = variantEntry.getValue();
            if (key.equals("model")) {

                ResourceLocation modelLocation = new ResourceLocation(value.getAsString());
                ResourceLocation convertedModelLocation = new ResourceLocation(modelLocation.getNamespace(), modelLocation.getPath() + "_" + Integer.toHexString(this.hashCode()));
                value = new JsonPrimitive(convertedModelLocation.toString());
                this.makeModel(modelLocation, convertedModelLocation);
            }

            newVariantObject.add(key, value);
        }

        return newVariantObject;
    }

    private void makeModel(ResourceLocation modelLocation, ResourceLocation convertedModelLocation) {

        // model might have been made before in case it's being used multiple times
        if (!this.resources.containsKey(AssetLocations.getBlockModelPath(convertedModelLocation))) {

            JsonElement modelElement = this.loadResource(AssetLocations.getBlockModelPath(modelLocation),
                    reader -> JsonConfigFileUtil.GSON.fromJson(reader, JsonElement.class));
            if (modelElement instanceof JsonObject) {

                List<BlockElement> elements = this.getConvertedModelElements(modelLocation);
                // read model from json once more, but this time we keep it as a json element
                modelElement.getAsJsonObject().add("elements", BlockElementAdapter.GSON.toJsonTree(elements));
                this.resources.put(AssetLocations.getBlockModelPath(convertedModelLocation), modelElement);
            }
        }
    }

    @SuppressWarnings("deprecation")
    private List<BlockElement> getConvertedModelElements(ResourceLocation modelLocation) {

        BlockModel blockModel = this.loadModel(modelLocation);
        // load all parent models which is important when getting elements
        blockModel.getMaterials(this::loadModel, Sets.newHashSet());
        List<BlockElement> elements = blockModel.getElements();
        // modify elements list using provided converter
        this.elementsConverter.accept(elements);

        return elements;
    }

    private BlockModel loadModel(ResourceLocation location) {

        return this.loadResource(AssetLocations.getBlockModelPath(location), reader -> {

            BlockModel blockmodel = BlockModel.fromStream(reader);
            // vanilla does it like that
            blockmodel.name = location.toString();
            return blockmodel;
        });
    }

    private <T> T loadResource(ResourceLocation jsonPath, Function<Reader, T> transform) {

        try (InputStream inputstream = this.resourceManager.getResource(jsonPath).getInputStream()) {

            InputStreamReader reader = new InputStreamReader(inputstream, StandardCharsets.UTF_8);
            return transform.apply(reader);
        } catch (IOException e) {

            DiagonalFences.LOGGER.warn("Exception loading resource definition: {}: {}", jsonPath, e);
        }

        return null;
    }

}
