package fuzs.diagonalfences.resources;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import net.minecraft.resources.*;
import net.minecraft.server.packs.metadata.MetadataSectionSerializer;
import net.minecraft.server.packs.metadata.pack.PackMetadataSection;
import net.minecraft.SharedConstants;
import net.minecraft.network.chat.TextComponent;
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

import net.minecraft.server.packs.PackResources;
import net.minecraft.server.packs.PackType;
import net.minecraft.server.packs.repository.Pack;
import net.minecraft.server.packs.repository.PackCompatibility;
import net.minecraft.server.packs.repository.PackSource;

@SuppressWarnings("NullableProblems")
public class RuntimeResourcePack implements PackResources, IResourceInfoFactory {

    private final Map<ResourceLocation, byte[]> resources = Maps.newHashMap();
    private final IResourceGenerator generator;
    private final String name;
    private final TextComponent description;
    private boolean locked;

    public RuntimeResourcePack(IResourceGenerator generator, String name, String description) {

        this.generator = generator;
        this.name = name;
        this.description = new TextComponent(description);
    }

    @Override
    public InputStream getRootResource(String fileName) throws IOException {

        throw new FileNotFoundException();
    }

    @Override
    public InputStream getResource(PackType type, ResourceLocation location) throws IOException {

        return new ByteArrayInputStream(this.getData(location));
    }

    private byte[] getData(ResourceLocation location) throws IOException {

        byte[] data = this.resources.get(location);
        if (data == null) {

            Map<ResourceLocation, byte[]> unitResources = this.getGeneratorData(location);
            this.resources.putAll(unitResources);
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
    public Collection<ResourceLocation> getResources(PackType type, String namespaceIn, String pathIn, int maxDepthIn, Predicate<String> filterIn) {

        if (!this.locked && type == PackType.CLIENT_RESOURCES) {

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
    public boolean hasResource(PackType type, ResourceLocation location) {

        return !this.locked && type == PackType.CLIENT_RESOURCES && this.getAllResourceLocations().contains(location);
    }

    @Override
    public Set<String> getNamespaces(PackType type) {

        if (!this.locked && type == PackType.CLIENT_RESOURCES) {

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
    public <T> T getMetadataSection(MetadataSectionSerializer<T> deserializer) {

        if (deserializer == PackMetadataSection.SERIALIZER) {

            return (T) new PackMetadataSection(this.description, SharedConstants.getCurrentVersion().getPackVersion());
        }

        return null;
    }

    @Override
    public String getName() {

        return this.name;
    }

    @Override
    public TextComponent getDescription() {

        return this.description;
    }

    @Override
    public Pack createResourcePack(String owner, boolean alwaysEnabled, Pack.Position position, boolean orderLocked, boolean hidden) {

        return new Pack(owner, alwaysEnabled, () -> this, new TextComponent(this.getName()),
                this.getDescription(), PackCompatibility.COMPATIBLE, position, orderLocked, PackSource.BUILT_IN, hidden);
    }

    private Collection<ResourceLocation> getAllResourceLocations() {

        return Stream.concat(this.resources.keySet().stream(), this.generator.getResourceLocations().stream())
                .collect(Collectors.toSet());
    }

}
