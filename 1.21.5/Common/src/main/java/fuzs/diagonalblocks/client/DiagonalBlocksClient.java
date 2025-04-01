package fuzs.diagonalblocks.client;

import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.handler.DiagonalModelHandler;
import fuzs.diagonalblocks.client.resources.translator.WallMultiPartTranslator;
import fuzs.diagonalblocks.client.resources.translator.WindowMultiPartTranslator;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.core.v1.context.BlockStateResolverContext;
import fuzs.puzzleslib.api.client.event.v1.ClientLifecycleEvents;
import fuzs.puzzleslib.api.client.renderer.v1.RenderTypeHelper;
import fuzs.puzzleslib.api.client.renderer.v1.model.ModelLoadingHelper;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BlockStateModel;
import net.minecraft.client.resources.model.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

public class DiagonalBlocksClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerLoadingHandlers();
        // this cannot happen later during client setup,
        // as model loading will already have begun by then in the background
        MultiPartTranslator.register(DiagonalBlockTypes.WINDOW, new WindowMultiPartTranslator());
        MultiPartTranslator.register(DiagonalBlockTypes.WALL, new WallMultiPartTranslator());
    }

    private static void registerLoadingHandlers() {
        ClientLifecycleEvents.STARTED.register((Minecraft minecraft) -> {
            // run a custom implementation here, the appropriate method in client mod constructor runs together with other mods, so we might miss some entries
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                for (Map.Entry<Block, Block> entry : type.getBlockConversions().entrySet()) {
                    RenderType renderType = RenderTypeHelper.getRenderType(entry.getKey());
                    RenderTypeHelper.registerRenderType(entry.getValue(), renderType);
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
                        (BlockStateResolverContext.ResourceLoaderContext resourceLoaderContext) -> {
                            return ModelLoadingHelper.loadBlockState(resourceLoaderContext.resourceManager(),
                                            BuiltInRegistries.BLOCK.getKey(oldBlock),
                                            resourceLoaderContext.executor())
                                    .thenApply((List<BlockStateModelLoader.LoadedBlockModelDefinition> loadedBlockModelDefinitions) -> {
                                        return DiagonalModelHandler.transformLoadedBlockModelDefinitions(
                                                loadedBlockModelDefinitions,
                                                multiPartTranslator, () -> {
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
                        (BlockStateModelLoader.LoadedModels loadedModels, BiConsumer<BlockState, BlockStateModel.UnbakedRoot> blockStateConsumer) -> {
                            for (BlockState blockState : newBlock.getStateDefinition().getPossibleStates()) {
                                if (loadedModels.models().containsKey(blockState)) {
                                    blockStateConsumer.accept(blockState, loadedModels.models().get(blockState));
                                } else {
                                    blockStateConsumer.accept(blockState, missingModel());
                                }
                            }
                        });
            });
        }
    }

    @Deprecated
    public static BlockStateModel.UnbakedRoot missingModel() {
        return new BlockStateModel.UnbakedRoot() {
            @Override
            public BlockStateModel bake(BlockState blockState, ModelBaker modelBaker) {
                UnbakedModel unbakedModel = MissingBlockModel.missingModel();
                // just use this, so we do not have to dealt with the internal resolved model implementation
                ResolvedModel resolvedModel = new ModelDiscovery(Collections.emptyMap(), unbakedModel).missingModel();
                return ModelBakery.MissingModels.bake(resolvedModel, modelBaker.sprites()).block();
            }

            @Override
            public Object visualEqualityGroup(BlockState state) {
                return this;
            }

            @Override
            public void resolveDependencies(ResolvableModel.Resolver resolver) {
                // NO-OP
            }
        };
    }
}
