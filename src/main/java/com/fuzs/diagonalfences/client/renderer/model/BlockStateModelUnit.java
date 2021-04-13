package com.fuzs.diagonalfences.client.renderer.model;

import com.fuzs.diagonalfences.DiagonalFences;
import com.fuzs.puzzleslib_df.client.json.adapter.BlockPartAdapter;
import com.fuzs.puzzleslib_df.json.JsonConfigFileUtil;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.model.BlockModel;
import net.minecraft.client.renderer.model.BlockPart;
import net.minecraft.resources.IResourceManager;
import net.minecraft.state.Property;
import net.minecraft.util.ResourceLocation;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

public class BlockStateModelUnit {

    private final IResourceManager resourceManager;
    public final Block block;
    @Nonnull
    public final ResourceLocation blockLocation;
    private final Map<ResourceLocation, JsonElement> resources = Maps.newHashMap();
    private final Map<String, String> propertyConverter;
    private final Consumer<List<BlockPart>> elementsConverter;

    @SuppressWarnings("ConstantConditions")
    public BlockStateModelUnit(IResourceManager resourceManager, Block block, Map<Property<?>, Property<?>> propertyConverter, Consumer<List<BlockPart>> elementsConverter) {

        this.resourceManager = resourceManager;
        this.block = block;
        this.blockLocation = block.getRegistryName();
        this.propertyConverter = propertyConverter.entrySet().stream()
                .collect(Collectors.toMap(entry -> entry.getKey().getName(), entry -> entry.getValue().getName()));
        this.elementsConverter = elementsConverter;
    }

    public Map<ResourceLocation, JsonElement> load() {

        this.resources.clear();

        JsonElement blockStates = this.loadResource(AssetLocations.getBlockStatesPath(this.blockLocation), reader -> JsonConfigFileUtil.GSON.fromJson(reader, JsonElement.class));
        if (blockStates instanceof JsonObject && blockStates.getAsJsonObject().has("multipart")) {

            JsonArray selectorArray = blockStates.getAsJsonObject().getAsJsonArray("multipart");
            // will be added after iterating
            List<JsonElement> additionalSelectors = Lists.newArrayList();
            for (JsonElement jsonElement : selectorArray) {

                JsonObject multipartObject = jsonElement.getAsJsonObject();
                if (multipartObject.has("when")) {

                    JsonObject whenObject = multipartObject.getAsJsonObject("when");
                    boolean isConvertible = whenObject.entrySet().stream()
                            .map(Map.Entry::getKey)
                            .anyMatch(this.propertyConverter::containsKey);

                    if (isConvertible) {

                        JsonObject convertedConditionObject = this.getConvertedCondition(whenObject);
                        // apply must be present if we've come this far
                        JsonElement applyElement = multipartObject.get("apply");
                        JsonElement convertedVariantElement;
                        if (applyElement.isJsonArray()) {

                            JsonArray variantList = new JsonArray();
                            for (JsonElement variantElement : applyElement.getAsJsonArray()) {

                                variantList.add(this.getConvertedVariant(variantElement));
                            }

                            convertedVariantElement = variantList;
                        } else {

                            convertedVariantElement = this.getConvertedVariant(applyElement);
                        }

                        JsonObject selectorObject = new JsonObject();
                        selectorObject.add("when", convertedConditionObject);
                        selectorObject.add("apply", convertedVariantElement);
                        additionalSelectors.add(selectorObject);
                    }
                }
            }

            additionalSelectors.forEach(selectorArray::add);
            this.resources.put(AssetLocations.getBlockStatesPath(this.blockLocation), blockStates);
        }

        return ImmutableMap.copyOf(this.resources);
    }

    private JsonElement getConvertedVariant(JsonElement variantElement) {

        JsonObject convertedVariantObject = new JsonObject();
        for (Map.Entry<String, JsonElement> entry : variantElement.getAsJsonObject().entrySet()) {

            String key = entry.getKey();
            JsonElement value = entry.getValue();
            if (key.equals("model")) {

                ResourceLocation modelLocation = new ResourceLocation(value.getAsString());
                ResourceLocation convertedModelLocation = new ResourceLocation(modelLocation.getNamespace(), modelLocation.getPath() + "_" + Integer.toHexString(this.hashCode()));
                value = new JsonPrimitive(convertedModelLocation.toString());
                if (!this.resources.containsKey(AssetLocations.getBlockModelPath(convertedModelLocation))) {

                    JsonElement modelElement = this.loadResource(AssetLocations.getBlockModelPath(modelLocation),
                            reader -> JsonConfigFileUtil.GSON.fromJson(reader, JsonElement.class));
                    if (modelElement instanceof JsonObject) {

                        BlockModel blockModel = this.loadModel(modelLocation);
                        // load all parent models which is important when getting elements
                        blockModel.getTextures(this::loadModel, Sets.newHashSet());
                        List<BlockPart> elements = blockModel.getElements();
                        // modify elements list using provided converter
                        DiagonalFences.LOGGER.info(modelLocation);
                        this.elementsConverter.accept(elements);
                        // read model from json once more, but this time we keep it as a json element
                        modelElement.getAsJsonObject().add("elements", BlockPartAdapter.GSON.toJsonTree(elements));
                        this.resources.put(AssetLocations.getBlockModelPath(convertedModelLocation), modelElement);
                    }
                }
            }

            convertedVariantObject.add(key, value);
        }

        return convertedVariantObject;
    }

    private JsonObject getConvertedCondition(JsonObject whenObject) {

        JsonObject convertedWhenObject = new JsonObject();
        for (Map.Entry<String, JsonElement> entry : whenObject.entrySet()) {

            String property = entry.getKey();
            property = this.propertyConverter.getOrDefault(property, property);
            convertedWhenObject.add(property, entry.getValue());
        }

        return convertedWhenObject;
    }

    private BlockModel loadModel(ResourceLocation location) {

        return this.loadResource(AssetLocations.getBlockModelPath(location), reader -> {

            BlockModel blockmodel = BlockModel.deserialize(reader);
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
