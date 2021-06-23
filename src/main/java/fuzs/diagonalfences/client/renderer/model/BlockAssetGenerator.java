package fuzs.diagonalfences.client.renderer.model;

import fuzs.diagonalfences.resources.IResourceGenerator;
import fuzs.diagonalfences.client.util.AssetLocations;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Maps;
import com.google.gson.JsonElement;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.model.BlockPart;
import net.minecraft.resources.IResourceManager;
import net.minecraft.util.ResourceLocation;
import org.apache.commons.lang3.tuple.Pair;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@SuppressWarnings("UnusedReturnValue")
public class BlockAssetGenerator implements IResourceGenerator {

    private final IResourceManager resourceManager;
    private final Map<ResourceLocation, Supplier<Map<ResourceLocation, JsonElement>>> resources = Maps.newHashMap();

    public BlockAssetGenerator(IResourceManager resourceManager) {

        this.resourceManager = resourceManager;
    }

    public IResourceGenerator addUnits(Collection<Block> blocks, Map<Pair<String, String>, String> propertyConverter, Consumer<List<BlockPart>> elementsConverter) {

        for (Block block : blocks) {

            this.addUnit(block, propertyConverter, elementsConverter);
        }

        return this;
    }

    public IResourceGenerator addUnit(Block block, Map<Pair<String, String>, String> propertyConverter, Consumer<List<BlockPart>> elementsConverter) {

        BlockStateModelUnit unit = new BlockStateModelUnit(this.resourceManager, block, propertyConverter, elementsConverter);
        this.resources.put(AssetLocations.getBlockStatesPath(unit.blockLocation), unit::load);

        return this;
    }

    public BlockAssetGenerator addResource(ResourceLocation path, JsonElement resourceElement) {

        this.resources.put(path, () -> Collections.singletonMap(path, resourceElement));
        return this;
    }

    @Override
    public boolean hasResourceLocation(ResourceLocation resource) {

        return this.resources.containsKey(resource);
    }

    @Override
    public Collection<ResourceLocation> getResourceLocations() {

        return ImmutableSet.copyOf(this.resources.keySet());
    }

    @Override
    public Map<ResourceLocation, byte[]> getResource(ResourceLocation resource) {

        if (this.hasResourceLocation(resource)) {

            return this.convert(this.resources.get(resource).get());
        }

        return Collections.emptyMap();
    }

    @Override
    public Map<ResourceLocation, byte[]> getResources() {

        return this.resources.values().stream()
                .map(Supplier::get)
                .map(this::convert)
                .flatMap(resources -> resources.entrySet().stream())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    private Map<ResourceLocation, byte[]> convert(Map<ResourceLocation, JsonElement> resources) {

        return resources.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> IResourceGenerator.toBytes(entry.getValue())));
    }

}
