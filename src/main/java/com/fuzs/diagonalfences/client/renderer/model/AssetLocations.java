package com.fuzs.diagonalfences.client.renderer.model;

import net.minecraft.util.ResourceLocation;

public class AssetLocations {

    public static ResourceLocation getBlockStatesPath(ResourceLocation location) {

        return new ResourceLocation(location.getNamespace(), "blockstates/" + location.getPath() + ".json");
    }

    public static ResourceLocation getBlockModelName(ResourceLocation location) {

        return new ResourceLocation(location.getNamespace(), "block/" + location.getPath());
    }

    public static ResourceLocation getBlockModelPath(ResourceLocation location) {

        location = getBlockModelName(location);
        return new ResourceLocation(location.getNamespace(), "models/" + location.getPath() + ".json");
    }

}
