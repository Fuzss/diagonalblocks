package com.fuzs.diagonalfences.client.renderer.model;

import com.fuzs.diagonalfences.DiagonalFences;
import com.fuzs.diagonalfences.data.BlockAssetProvider;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.gson.*;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.model.BlockModel;
import net.minecraft.client.renderer.model.RenderMaterial;
import net.minecraft.client.renderer.model.Variant;
import net.minecraft.client.renderer.model.VariantList;
import net.minecraft.client.renderer.texture.MissingTextureSprite;
import net.minecraft.data.IFinishedBlockState;
import net.minecraft.resources.IResourceManager;
import net.minecraft.state.Property;
import net.minecraft.util.JSONUtils;
import net.minecraft.util.ResourceLocation;
import org.apache.commons.lang3.ArrayUtils;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

public class BlockStateModelUnit {

    private static final Gson GSON = new GsonBuilder()
            .registerTypeAdapter(Variant.class, new Variant.Deserializer())
            .registerTypeAdapter(VariantList.class, new VariantList.Deserializer())
            .create();

    private final IResourceManager resourceManager;
    public final Block block;
    @Nonnull
    public final ResourceLocation blockLocation;
    private final ModelData data;
    private final Map<ResourceLocation, JsonElement> resources = Maps.newHashMap();

    @SuppressWarnings("ConstantConditions")
    public BlockStateModelUnit(IResourceManager resourceManager, Block block, ModelData data) {

        this.resourceManager = resourceManager;
        this.block = block;
        this.blockLocation = block.getRegistryName();
        this.data = data;
    }

    public Map<ResourceLocation, JsonElement> load() {

        this.resources.clear();
        JsonObject jsonObject = this.getBlockStateResource();
        if (jsonObject != null && jsonObject.has("multipart")) {

            JsonArray multipart = JSONUtils.getJsonArray(jsonObject, "multipart");
            if (this.addVariantModels(multipart)) {

                this.addBlockStates(multipart);
                this.resources.put(AssetLocations.getBlockStatesPath(this.blockLocation), jsonObject);
            }
        }

        return ImmutableMap.copyOf(this.resources);
    }

    private void addBlockStates(JsonArray multipartArray) {

        ResourceLocation[] modelNames = Stream.of(this.data.newProperties)
                .map(property -> AssetLocations.getBlockModelName(this.getModelName(property)))
                .toArray(ResourceLocation[]::new);
        IFinishedBlockState diagonalState = BlockAssetProvider.getDiagonalState(this.block, modelNames);
        JsonObject toBeAdded = diagonalState.get().getAsJsonObject();
        multipartArray.addAll(JSONUtils.getJsonArray(toBeAdded, "multipart"));
    }

    private JsonObject getBlockStateResource() {

        JsonElement stateElement = this.loadResource(AssetLocations.getBlockStatesPath(this.blockLocation), reader -> GSON.fromJson(reader, JsonElement.class));
        if (stateElement != null && stateElement.isJsonObject()) {

            return stateElement.getAsJsonObject();
        }

        return null;
    }

    private boolean addVariantModels(JsonArray multipartArray) {

        ResourceLocation[] textures = Stream.of(this.data.referenceProperties)
                .map(property -> this.getPropertyTexture(multipartArray, property))
                .toArray(ResourceLocation[]::new);

        // just being extra safe cause you never know with other mods
        applyFallback(textures);
        if (ArrayUtils.isNotEmpty(textures)) {

            for (int i = 0; i < textures.length; i++) {

                ResourceLocation model = AssetLocations.getBlockModelPath(this.getModelName(this.data.newProperties[i]));
                this.resources.put(model, BlockAssetProvider.getVariantModel(this.data.baseModel, textures[i]));
            }

            return true;
        }

        DiagonalFences.LOGGER.warn("Unable to create variant models");
        return false;
    }

    private ResourceLocation getModelName(Property<?> property) {

        return new ResourceLocation(this.blockLocation.getNamespace(), this.blockLocation.getPath() + "_" + property.getName() + "_" + this.data.modelSuffix);
    }

    private ResourceLocation getPropertyTexture(JsonArray multipartArray, Property<?> property) {

        Optional<ResourceLocation> propertyModel = getPropertyModel(multipartArray, property);
        return propertyModel.map(this::getModelTexture).orElse(null);

    }

    private ResourceLocation getModelTexture(ResourceLocation location) {

        BlockModel blockModel = this.loadModel(location);
        ResourceLocation texture = MissingTextureSprite.getLocation();
        if (blockModel != null) {

            texture = blockModel.resolveTextureName("texture").getTextureLocation();
            if (MissingTextureSprite.getLocation().equals(texture)) {

                // get any other texture if none was found for 'texture'
                Collection<RenderMaterial> allTextures = blockModel.getTextures(this::loadModel, Sets.newHashSet());
                Optional<ResourceLocation> otherTexture = allTextures.stream()
                        .map(RenderMaterial::getTextureLocation)
                        .filter(material -> !MissingTextureSprite.getLocation().equals(material))
                        .findAny();
                if (otherTexture.isPresent()) {

                    texture = otherTexture.get();
                }
            }
        }

        return texture;
    }

    private BlockModel loadModel(ResourceLocation location) {

        ResourceLocation jsonPath = new ResourceLocation(location.getNamespace(), "models/" + location.getPath() + ".json");
        return this.loadResource(jsonPath, reader -> {

            BlockModel blockmodel = BlockModel.deserialize(reader);
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

    private static Optional<ResourceLocation> getPropertyModel(JsonArray multipartArray, Property<?> property) {

        String name = property.getName();
        for (JsonElement multipartElement : multipartArray) {

            if (multipartElement.isJsonObject()) {

                JsonObject multipartObject = multipartElement.getAsJsonObject();
                if (multipartObject.has("when")) {

                    JsonObject conditionObject = JSONUtils.getJsonObject(multipartObject, "when");
                    if (conditionObject.has(name) && JSONUtils.getBoolean(conditionObject, name)) {

                        // apply must be present if we've come this far
                        VariantList variants = GSON.fromJson(multipartObject.get("apply"), VariantList.class);
                        if (!variants.getDependencies().isEmpty()) {

                            return variants.getDependencies().stream().findAny();
                        }
                    }
                }
            }
        }

        return Optional.empty();
    }

    private static <T> void applyFallback(T[] models) {

        final T fallback = Stream.of(models)
                .filter(Objects::nonNull)
                .findAny()
                .orElse(null);

        // set every null value to fallback we just found
        for (int i = 0; i < models.length; i++) {

            if (models[i] == null) {

                models[i] = fallback;
            }
        }
    }

    public static class ModelData {

        private final ResourceLocation baseModel;
        private final String modelSuffix;
        private final Property<?>[] newProperties;
        private final Property<?>[] referenceProperties;

        public ModelData(ResourceLocation baseModel, String modelSuffix, Property<?>[] newProperties, Property<?>[] referenceProperties) {

            this.baseModel = baseModel;
            this.modelSuffix = modelSuffix;
            this.newProperties = newProperties;
            this.referenceProperties = referenceProperties;
        }

    }

}
