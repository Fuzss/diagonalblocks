package fuzs.diagonalblocks.client.handler;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.data.ModBlockTagsProvider;
import net.minecraft.client.renderer.block.model.BlockModelDefinition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.BlockStateModelLoader;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.BiConsumer;

public class DiagonalModelHandler {
    static final Set<ResourceLocation> INVALID_MODEL_BLOCKS = Collections.newSetFromMap(new WeakHashMap<>());

    public static List<BlockStateModelLoader.LoadedBlockModelDefinition> transformLoadedBlockModelDefinitions(List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions, MultiPartTranslator multiPartTranslator, BiConsumer<ResourceLocation, UnbakedModel> modelAdder, Runnable invalidBlockModelReporter) {
        List<BlockStateModelLoader.LoadedBlockModelDefinition> newLoadedBlockModelDefinitions = new ArrayList<>(
                loadedBlockModelDefinitions.size());
        for (BlockStateModelLoader.LoadedBlockModelDefinition loadedBlockModelDefinition : loadedBlockModelDefinitions) {
            // the rotated model parts must be added to the global models map, which does not exist yet, so we store them here temporarily
            // we are able to create those models already, as they only require the vanilla model parts they are rotating during baking,
            // when vanilla has loaded all unbaked models
            BlockModelDefinition blockModelDefinition = transformBlockModelDefinition(loadedBlockModelDefinition.contents(),
                    multiPartTranslator,
                    modelAdder);
            if (blockModelDefinition != null) {
                newLoadedBlockModelDefinitions.add(new BlockStateModelLoader.LoadedBlockModelDefinition(
                        loadedBlockModelDefinition.source(),
                        blockModelDefinition));
            }
            if (blockModelDefinition == null || blockModelDefinition == loadedBlockModelDefinition.contents()) {
                invalidBlockModelReporter.run();
            }
        }
        return newLoadedBlockModelDefinitions;
    }

    @Nullable
    static BlockModelDefinition transformBlockModelDefinition(BlockModelDefinition blockModelDefinition, MultiPartTranslator multiPartTranslator, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        MultiPart.Definition multiPart = blockModelDefinition.getMultiPart();
        if (multiPart != null) {
            // the rotated model parts must be added to the global models map, which does not exist yet, so we store them here temporarily
            // we are able to create those models aready, as they only require the vanilla model parts they are rotating during baking,
            // when vanilla has loaded all unbaked models
            return new BlockModelDefinition(Collections.emptyMap(), multiPartTranslator.apply(multiPart, modelAdder));
        } else {
            if (multiPartTranslator.allowBaseModelAsFallback()) {
                return blockModelDefinition;
            } else {
                return null;
            }
        }
    }

    public static void reportInvalidBlockModel(ResourceLocation resourceLocation, DiagonalBlockType blockType) {
        if (INVALID_MODEL_BLOCKS.add(resourceLocation)) {
            // don't report built-in blacklisted blocks
            if (!ModBlockTagsProvider.TAG_BLACKLISTED_TYPES.getOrDefault(blockType, Collections.emptyList())
                    .contains(resourceLocation.toString())) {
                DiagonalBlocks.LOGGER.warn(
                        "Block '{}' is not using multipart model and should be added to the '{}' block tag. The model will not appear correctly in-game!",
                        resourceLocation,
                        blockType.getBlacklistTagKey().location());
            }
        }
    }
}
