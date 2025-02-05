package fuzs.diagonalblocks.client.handler;

import com.google.common.base.Suppliers;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Sets;
import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.data.ModBlockTagsProvider;
import net.minecraft.Util;
import net.minecraft.client.renderer.block.model.BlockModelDefinition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.BlockStateModelLoader;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.client.resources.model.ModelManager;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Supplier;

public class DiagonalModelHandler {
    private static final Supplier<Map<DiagonalBlockType, Map<ResourceLocation, ResourceLocation>>> BLOCK_CONVERSION_RESOURCE_LOCATIONS;
    private static final Set<ResourceLocation> NO_MULTIPART_MODEL_BLOCKS = Sets.newHashSet();
    private static final Map<ResourceLocation, UnbakedModel> ADDITIONAL_BLOCK_MODELS = new ConcurrentHashMap<>();

    static {
        BLOCK_CONVERSION_RESOURCE_LOCATIONS = Suppliers.memoize(() -> DiagonalBlockType.TYPES.stream()
                .collect(ImmutableMap.toImmutableMap(Function.identity(), type -> {
                    return type.getBlockConversions().entrySet().stream().map(
                            (Map.Entry<Block, Block> entry) -> Map.entry(
                                    entry.getKey().builtInRegistryHolder().key().location(),
                                    entry.getValue().builtInRegistryHolder().key().location()
                            )).collect(Util.toMap());
                })));
    }

    @Nullable
    public static BlockStateModelLoader.LoadedModels loadBlockStateDefinition(BlockStateModelLoader blockStateModelLoader, ResourceLocation resourceLocation, Function<ResourceLocation, StateDefinition<Block, BlockState>> stateDefinitionGetter, BlockModelDefinition blockModelDefinition) {
        // this is patched in where block state definitions are normally loaded, and is intended to capture non-diagonal definitions
        // we need the block state definitions at this stage, as they still contain MultiPart$Definition,
        // before it is converted to MultiPart, which no longer contains the raw selector instances that we need
        for (Map.Entry<DiagonalBlockType, Map<ResourceLocation, ResourceLocation>> entry : BLOCK_CONVERSION_RESOURCE_LOCATIONS.get()
                .entrySet()) {
            if (entry.getValue().containsKey(resourceLocation)) {
                DiagonalBlockType blockType = entry.getKey();
                ResourceLocation diagonalResourceLocation = entry.getValue().get(resourceLocation);
                StateDefinition<Block, BlockState> stateDefinition = stateDefinitionGetter.apply(
                        diagonalResourceLocation);
                MultiPart.Definition multiPart = blockModelDefinition.getMultiPart();
                MultiPartTranslator multiPartTranslator = MultiPartTranslator.get(blockType);
                if (multiPart != null) {
                    // the rotated model parts must be added to the global models map, which does not exist yet, so we store them here temporarily
                    // we are able to create those models aready, as they only require the vanilla model parts they are rotating during baking,
                    // when vanilla has loaded all unbaked models
                    MultiPart.Definition diagonalMultiPart = multiPartTranslator.apply(multiPart,
                            ADDITIONAL_BLOCK_MODELS::put
                    );
                    BlockModelDefinition diagonalBlockModelDefinition = new BlockModelDefinition(Collections.emptyMap(),
                            diagonalMultiPart
                    );
                    return loadBlockStateDefinition(blockStateModelLoader, diagonalResourceLocation, stateDefinition,
                            diagonalBlockModelDefinition, blockType.toString()
                    );
                } else {
                    reportNoMultiPartModel(resourceLocation, blockType);
                    if (multiPartTranslator.allowBaseModelAsFallback()) {
                        return loadBlockStateDefinition(blockStateModelLoader, diagonalResourceLocation,
                                stateDefinition, blockModelDefinition, blockType.toString()
                        );
                    } else {
                        return null;
                    }
                }
            }
        }

        return null;
    }

    private static BlockStateModelLoader.LoadedModels loadBlockStateDefinition(BlockStateModelLoader blockStateModelLoader, ResourceLocation resourceLocation, StateDefinition<Block, BlockState> stateDefinition, BlockModelDefinition blockModelDefinition, String blockModelSource) {
        List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions = List.of(
                new BlockStateModelLoader.LoadedBlockModelDefinition(blockModelSource, blockModelDefinition));
        return blockStateModelLoader.loadBlockStateDefinitionStack(resourceLocation, stateDefinition,
                loadedBlockModelDefinitions
        );
    }

    private static void reportNoMultiPartModel(ResourceLocation resourceLocation, DiagonalBlockType blockType) {
        if (NO_MULTIPART_MODEL_BLOCKS.add(resourceLocation)) {
            // don't report built-in blacklisted blocks
            if (!ModBlockTagsProvider.TAG_BLACKLISTED_TYPES.getOrDefault(blockType, Collections.emptyList()).contains(
                    resourceLocation.toString())) {
                DiagonalBlocks.LOGGER.warn(
                        "Block '{}' is not using multipart model and should be added to the '{}' block tag. The model will not appear correctly under some circumstances!",
                        resourceLocation, blockType.getBlacklistTagKey().location()
                );
            }
        }
    }

    public static Map<ResourceLocation, UnbakedModel> putAdditionalBlockModels(Map<ResourceLocation, UnbakedModel> blockModels) {
        if (!DiagonalModelHandler.ADDITIONAL_BLOCK_MODELS.isEmpty()) {
            blockModels = new HashMap<>(blockModels);
            blockModels.putAll(DiagonalModelHandler.ADDITIONAL_BLOCK_MODELS);
            return blockModels;
        } else {
            return blockModels;
        }
    }

    public static void onCompleteModelLoading(Supplier<ModelManager> modelManager, Supplier<ModelBakery> modelBakery) {
        ADDITIONAL_BLOCK_MODELS.clear();
    }
}
