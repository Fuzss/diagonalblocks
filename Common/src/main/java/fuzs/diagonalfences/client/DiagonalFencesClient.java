package fuzs.diagonalfences.client;

import com.google.common.collect.Sets;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.core.ClientModServices;
import fuzs.diagonalfences.client.model.MultipartAppender;
import fuzs.puzzleslib.client.core.ClientModConstructor;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.client.resources.model.ModelManager;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockState;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class DiagonalFencesClient implements ClientModConstructor {

    @Override
    public void onLoadModels(ModelManager modelManager, Map<ResourceLocation, BakedModel> models, ModelBakery modelBakery) {
        onBakingCompleted(modelManager, models, modelBakery);
    }

    private static void onBakingCompleted(ModelManager modelManager, Map<ResourceLocation, BakedModel> models, ModelBakery modelBakery) {
        Set<Block> erroredBlocks = Sets.newHashSet();
        Registry.BLOCK.stream()
                .filter(block -> block instanceof FenceBlock)
                .filter(block -> block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties())
                .flatMap(block -> block.getStateDefinition().getPossibleStates().stream())
                .forEach(state -> {
                    ResourceLocation fenceLocation = BlockModelShaper.stateToModelLocation(state);
                    BakedModel model = models.get(fenceLocation);
                    Optional<MultiPartBakedModel> multiPartBakedModel = ClientModServices.ABSTRACTIONS.getMultiPartBakedModel(model);
                    if (multiPartBakedModel.isPresent()) {
                        appendDiagonalFenceSelectors(state.getBlock(), multiPartBakedModel.get());
                    } else if (!erroredBlocks.contains(state.getBlock())){
                        erroredBlocks.add(state.getBlock());
                        DiagonalFences.LOGGER.info("Fence block '{}' is not using multipart models, diagonal fence connections may not be visible!", state.getBlock());
                    }
                });
    }

    private static Optional<BakedModel> appendDiagonalFenceSelectors(Block block, MultiPartBakedModel model) {
        Map<BlockState, Direction> oneArmStates = Map.of(
                block.defaultBlockState().setValue(FenceBlock.NORTH, true), Direction.NORTH,
                block.defaultBlockState().setValue(FenceBlock.EAST, true), Direction.EAST,
                block.defaultBlockState().setValue(FenceBlock.SOUTH, true), Direction.SOUTH,
                block.defaultBlockState().setValue(FenceBlock.WEST, true), Direction.WEST
        );
        List<BlockState> testStates = List.of(
                block.defaultBlockState().setValue(DiagonalBlock.NORTH_EAST, true),
                block.defaultBlockState().setValue(DiagonalBlock.NORTH_WEST, true),
                block.defaultBlockState().setValue(DiagonalBlock.SOUTH_EAST, true),
                block.defaultBlockState().setValue(DiagonalBlock.SOUTH_WEST, true)
        );
        return MultipartAppender.appendDiagonalSelectors(block, oneArmStates, model, testStates);
    }
}
