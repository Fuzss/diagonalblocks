package com.fuzs.diagonalfences.resources;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import net.minecraft.resources.*;
import net.minecraft.resources.data.IMetadataSectionSerializer;
import net.minecraft.resources.data.PackMetadataSection;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.SharedConstants;
import net.minecraft.util.text.StringTextComponent;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Nullable;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@SuppressWarnings("NullableProblems")
public class RuntimeResourcePack implements IResourcePack, IResourceInfoFactory {

    private final Map<ResourceLocation, byte[]> resources = Maps.newHashMap();
    private final IResourceGenerator generator;
    private final String name;
    private final StringTextComponent description;
    private boolean locked;

    public RuntimeResourcePack(IResourceGenerator generator, String name, String description) {

        this.generator = generator;
        this.name = name;
        this.description = new StringTextComponent(description);
    }

    @Override
    public InputStream getRootResourceStream(String fileName) throws IOException {

        throw new FileNotFoundException();
    }

    @Override
    public InputStream getResourceStream(ResourcePackType type, ResourceLocation location) throws IOException {

        return new ByteArrayInputStream(this.getData(location));
    }

    private byte[] getData(ResourceLocation location) throws IOException {

        byte[] data = this.resources.get(location);
        if (data == null) {

            Map<ResourceLocation, byte[]> unitResources = this.getGeneratorData(location);
            unitResources.forEach(this.resources::put);
            data = unitResources.get(location);
        }

        return data;
    }

    private Map<ResourceLocation, byte[]> getGeneratorData(ResourceLocation location) throws IOException {

        this.locked = true;
        Map<ResourceLocation, byte[]> unitResources = this.generator.getResource(location);
        this.locked = false;
        if (unitResources.isEmpty()) {

            throw new FileNotFoundException();
        }

        return unitResources;
    }

    @Override
    public Collection<ResourceLocation> getAllResourceLocations(ResourcePackType type, String namespaceIn, String pathIn, int maxDepthIn, Predicate<String> filterIn) {

        if (!this.locked && type == ResourcePackType.CLIENT_RESOURCES) {

            int currentDepth = StringUtils.countMatches(pathIn, "/");
            return this.getAllResourceLocations().stream()
                    .filter(location -> location.getNamespace().equals(namespaceIn))
                    .filter(location -> location.getPath().startsWith(pathIn))
                    .filter(location -> filterIn.test(location.getPath()))
                    .filter(location -> StringUtils.countMatches(location.getPath(), "/") - currentDepth <= maxDepthIn)
                    .collect(Collectors.toSet());
        }

        return Sets.newHashSet();
    }

    @Override
    public boolean resourceExists(ResourcePackType type, ResourceLocation location) {

        return !this.locked && type == ResourcePackType.CLIENT_RESOURCES && this.getAllResourceLocations().contains(location);
    }

    @Override
    public Set<String> getResourceNamespaces(ResourcePackType type) {

        if (!this.locked && type == ResourcePackType.CLIENT_RESOURCES) {

            return this.getAllResourceLocations().stream()
                    .map(ResourceLocation::getNamespace)
                    .collect(Collectors.toSet());
        }

        return Sets.newHashSet();
    }

    @Override
    public void close() {

        this.resources.clear();
    }

    @SuppressWarnings("unchecked")
    @Nullable
    @Override
    public <T> T getMetadata(IMetadataSectionSerializer<T> deserializer) {

        if (deserializer == PackMetadataSection.SERIALIZER) {

            return (T) new PackMetadataSection(this.description, SharedConstants.getVersion().getPackVersion());
        }

        return null;
    }

    @Override
    public String getName() {

        return this.name;
    }

    @Override
    public StringTextComponent getDescription() {

        return this.description;
    }

    @Override
    public ResourcePackInfo createResourcePack(String owner, boolean alwaysEnabled, ResourcePackInfo.Priority priority, boolean orderLocked, boolean hidden) {

        return new ResourcePackInfo(owner, alwaysEnabled, () -> this, new StringTextComponent(this.getName()),
                this.getDescription(), PackCompatibility.COMPATIBLE, priority, orderLocked, IPackNameDecorator.BUILTIN, hidden);
    }

    private Collection<ResourceLocation> getAllResourceLocations() {

        return Stream.concat(this.resources.keySet().stream(), this.generator.getResourceLocations().stream())
                .collect(Collectors.toSet());
    }

}
