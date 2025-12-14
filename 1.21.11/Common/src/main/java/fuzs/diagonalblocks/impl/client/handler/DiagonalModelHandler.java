package fuzs.diagonalblocks.impl.client.handler;

import fuzs.diagonalblocks.impl.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.impl.data.ModBlockTagsProvider;
import net.minecraft.client.renderer.block.model.BlockModelDefinition;
import net.minecraft.client.resources.model.BlockStateModelLoader;
import net.minecraft.resources.Identifier;
import org.jspecify.annotations.Nullable;

import java.util.*;

public class DiagonalModelHandler {
    static final Set<Identifier> INVALID_MODEL_BLOCKS = Collections.newSetFromMap(new WeakHashMap<>());

    public static List<BlockStateModelLoader.LoadedBlockModelDefinition> transformLoadedBlockModelDefinitions(List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions, MultiPartTranslator multiPartTranslator, Runnable invalidBlockModelReporter) {
        List<BlockStateModelLoader.LoadedBlockModelDefinition> newLoadedBlockModelDefinitions = new ArrayList<>(
                loadedBlockModelDefinitions.size());
        for (BlockStateModelLoader.LoadedBlockModelDefinition loadedBlockModelDefinition : loadedBlockModelDefinitions) {
            // the rotated model parts must be added to the global models map, which does not exist yet, so we store them here temporarily
            // we are able to create those models already, as they only require the vanilla model parts they are rotating during baking,
            // when vanilla has loaded all unbaked models
            BlockModelDefinition blockModelDefinition = transformBlockModelDefinition(loadedBlockModelDefinition.contents(),
                    multiPartTranslator);
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

    @Nullable static BlockModelDefinition transformBlockModelDefinition(BlockModelDefinition blockModelDefinition, MultiPartTranslator multiPartTranslator) {
        BlockModelDefinition.MultiPartDefinition multiPart = blockModelDefinition.multiPart().orElse(null);
        if (multiPart != null) {
            // the rotated model parts must be added to the global models map, which does not exist yet, so we store them here temporarily
            // we are able to create those models aready, as they only require the vanilla model parts they are rotating during baking,
            // when vanilla has loaded all unbaked models
            return new BlockModelDefinition(Optional.empty(),
                    Optional.of(multiPartTranslator.apply(multiPart)));
        } else {
            if (multiPartTranslator.allowBaseModelAsFallback()) {
                return blockModelDefinition;
            } else {
                return null;
            }
        }
    }

    public static void reportInvalidBlockModel(Identifier identifier, DiagonalBlockType blockType) {
        if (INVALID_MODEL_BLOCKS.add(identifier)) {
            // don't report built-in blacklisted blocks
            if (!ModBlockTagsProvider.TAG_BLACKLISTED_TYPES.getOrDefault(blockType, Collections.emptyList())
                    .contains(identifier.toString())) {
                DiagonalBlocks.LOGGER.warn(
                        "Block '{}' is not using multipart model and should be added to the '{}' block tag. The model will not appear correctly in-game!",
                        identifier,
                        blockType.getBlacklistTagKey().location());
            }
        }
    }
}
