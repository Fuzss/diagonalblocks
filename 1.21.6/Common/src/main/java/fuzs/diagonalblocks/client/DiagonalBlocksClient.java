package fuzs.diagonalblocks.client;

import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.handler.DiagonalModelHandler;
import fuzs.diagonalblocks.client.resources.translator.WallMultiPartTranslator;
import fuzs.diagonalblocks.client.resources.translator.WindowMultiPartTranslator;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.core.v1.context.BlockStateResolverContext;
import fuzs.puzzleslib.api.client.core.v1.context.RenderTypesContext;
import fuzs.puzzleslib.api.client.renderer.v1.model.ModelLoadingHelper;
import net.minecraft.client.renderer.block.model.BlockStateModel;
import net.minecraft.client.resources.model.BlockStateModelLoader;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;

import java.util.List;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.function.BiConsumer;

public class DiagonalBlocksClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        // this cannot happen later during client setup,
        // as model loading will already have begun by then in the background
        MultiPartTranslator.register(DiagonalBlockTypes.WINDOW, new WindowMultiPartTranslator());
        MultiPartTranslator.register(DiagonalBlockTypes.WALL, new WallMultiPartTranslator());
    }

    @Override
    public void onRegisterBlockStateResolver(BlockStateResolverContext context) {
        for (DiagonalBlockType diagonalBlockType : DiagonalBlockType.TYPES) {
            MultiPartTranslator multiPartTranslator = MultiPartTranslator.get(diagonalBlockType);
            diagonalBlockType.getBlockConversions().forEach((Block oldBlock, Block newBlock) -> {
                context.registerBlockStateResolver(newBlock,
                        (ResourceManager resourceManager, Executor executor) -> {
                            return ModelLoadingHelper.loadBlockState(resourceManager,
                                            BuiltInRegistries.BLOCK.getKey(oldBlock),
                                            executor)
                                    .thenApply((List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions) -> {
                                        return DiagonalModelHandler.transformLoadedBlockModelDefinitions(
                                                loadedBlockModelDefinitions,
                                                multiPartTranslator,
                                                () -> {
                                                    DiagonalModelHandler.reportInvalidBlockModel(BuiltInRegistries.BLOCK.getKey(
                                                            oldBlock), diagonalBlockType);
                                                });
                                    })
                                    .thenCompose((List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions) -> {
                                        return ModelLoadingHelper.loadBlockState(loadedBlockModelDefinitions,
                                                BuiltInRegistries.BLOCK.getKey(newBlock),
                                                newBlock.getStateDefinition(),
                                                executor);
                                    });
                        },
                        (BlockStateModelLoader.LoadedModels loadedModels, BiConsumer<BlockState, BlockStateModel.UnbakedRoot> blockStateConsumer) -> {
                            for (BlockState blockState : newBlock.getStateDefinition().getPossibleStates()) {
                                if (loadedModels.models().containsKey(blockState)) {
                                    blockStateConsumer.accept(blockState, loadedModels.models().get(blockState));
                                } else {
                                    blockStateConsumer.accept(blockState, ModelLoadingHelper.missingModel());
                                }
                            }
                        });
            });
        }
    }

    @Override
    public void onRegisterBlockRenderTypes(RenderTypesContext<Block> context) {
        // this runs deferred by default, so we should have all entries from other mods available to us
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            for (Map.Entry<Block, Block> entry : type.getBlockConversions().entrySet()) {
                context.registerChunkRenderType(entry.getValue(), context.getChunkRenderType(entry.getKey()));
            }
        }
    }
}
