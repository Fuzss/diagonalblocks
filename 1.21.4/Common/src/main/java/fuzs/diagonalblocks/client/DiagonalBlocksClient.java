package fuzs.diagonalblocks.client;

import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.handler.DiagonalModelHandler;
import fuzs.diagonalblocks.client.resources.translator.WallMultiPartTranslator;
import fuzs.diagonalblocks.client.resources.translator.WindowMultiPartTranslator;
import fuzs.puzzleslib.api.client.core.v1.ClientAbstractions;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.core.v1.context.BlockStateResolverContext;
import fuzs.puzzleslib.api.client.event.v1.ClientStartedCallback;
import fuzs.puzzleslib.api.client.util.v1.ModelLoadingHelper;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.UnbakedBlockStateModel;
import net.minecraft.client.resources.model.BlockStateModelLoader;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;

import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

public class DiagonalBlocksClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerEventHandlers();
        // this cannot happen later during client setup,
        // as model loading will already have begun by then in the background
        MultiPartTranslator.register(DiagonalBlockTypes.WINDOW, new WindowMultiPartTranslator());
        MultiPartTranslator.register(DiagonalBlockTypes.WALL, new WallMultiPartTranslator());
    }

    private static void registerEventHandlers() {
        ClientStartedCallback.EVENT.register((Minecraft minecraft) -> {
            // run a custom implementation here, the appropriate method in client mod constructor runs together with other mods, so we might miss some entries
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                for (Map.Entry<Block, Block> entry : type.getBlockConversions().entrySet()) {
                    RenderType renderType = ClientAbstractions.INSTANCE.getRenderType(entry.getKey());
                    ClientAbstractions.INSTANCE.registerRenderType(entry.getValue(), renderType);
                }
            }
        });
    }

    @Override
    public void onRegisterBlockStateResolver(BlockStateResolverContext context) {
        for (DiagonalBlockType diagonalBlockType : DiagonalBlockType.TYPES) {
            MultiPartTranslator multiPartTranslator = MultiPartTranslator.get(diagonalBlockType);
            diagonalBlockType.getBlockConversions().forEach((Block oldBlock, Block newBlock) -> {
                context.registerBlockStateResolver(newBlock,
                        resourceLoaderContext -> {
                            return ModelLoadingHelper.loadBlockState(resourceLoaderContext.resourceManager(),
                                            BuiltInRegistries.BLOCK.getKey(oldBlock),
                                            resourceLoaderContext.executor())
                                    .thenApply((List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions) -> {
                                        return DiagonalModelHandler.transformLoadedBlockModelDefinitions(
                                                loadedBlockModelDefinitions,
                                                multiPartTranslator,
                                                resourceLoaderContext::addModel,
                                                () -> {
                                                    DiagonalModelHandler.reportInvalidBlockModel(BuiltInRegistries.BLOCK.getKey(
                                                            oldBlock), diagonalBlockType);
                                                });
                                    })
                                    .thenCompose((List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions) -> {
                                        return ModelLoadingHelper.loadBlockState(loadedBlockModelDefinitions,
                                                BuiltInRegistries.BLOCK.getKey(newBlock),
                                                newBlock.getStateDefinition(),
                                                resourceLoaderContext.executor());
                                    });
                        },
                        (BlockStateModelLoader.LoadedModels loadedModels, BiConsumer<BlockState, UnbakedBlockStateModel> consumer) -> {
                            for (BlockState blockState : newBlock.getStateDefinition().getPossibleStates()) {
                                ModelResourceLocation modelResourceLocation = BlockModelShaper.stateToModelLocation(
                                        blockState);
                                if (loadedModels.models().containsKey(modelResourceLocation)) {
                                    consumer.accept(blockState,
                                            loadedModels.models().get(modelResourceLocation).model());
                                } else {
                                    consumer.accept(blockState, ModelLoadingHelper.missingModel());
                                }
                            }
                        });
            });
        }
    }
}
