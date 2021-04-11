package com.fuzs.diagonalfences.client.renderer.model;

import com.google.common.base.Preconditions;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.model.generators.CustomLoaderBuilder;
import net.minecraftforge.client.model.generators.ModelBuilder;
import net.minecraftforge.common.data.ExistingFileHelper;

import java.util.function.BiFunction;

/**
 * model builder without the need for {@link net.minecraftforge.common.data.ExistingFileHelper}
 * so it can be used independent of {@link net.minecraftforge.fml.event.lifecycle.GatherDataEvent}
 */
public class RuntimeModelBuilder extends ModelBuilder<RuntimeModelBuilder> {

    public RuntimeModelBuilder() {

        super(new ResourceLocation("empty"), null);
    }

    @Override
    public ResourceLocation getLocation() {

        throw new UnsupportedOperationException();
    }

    @Override
    public ResourceLocation getUncheckedLocation() {

        throw new UnsupportedOperationException();
    }

    @Override
    public RuntimeModelBuilder texture(String key, ResourceLocation texture) {

        Preconditions.checkNotNull(key, "Key must not be null");
        Preconditions.checkNotNull(texture, "Texture must not be null");
        this.textures.put(key, texture.toString());

        return this;
    }

    @Override
    public <L extends CustomLoaderBuilder<RuntimeModelBuilder>> L customLoader(BiFunction<RuntimeModelBuilder, ExistingFileHelper, L> customLoaderFactory) {

        throw new UnsupportedOperationException();
    }

}
