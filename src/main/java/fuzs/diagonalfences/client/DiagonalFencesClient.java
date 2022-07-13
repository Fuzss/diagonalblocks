package fuzs.diagonalfences.client;

import com.google.common.collect.Sets;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.model.MultipartAppender;
import fuzs.diagonalfences.client.model.MultipartDirectionData;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ModelBakeEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.List;
import java.util.Map;
import java.util.Set;

@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
public class DiagonalFencesClient {

    @SubscribeEvent
    public static void onModelsBaked(ModelBakeEvent event) {

        Map<ResourceLocation, BakedModel> registry = event.getModelRegistry();
        Set<Block> erroredBlocks = Sets.newHashSet();

        ForgeRegistries.BLOCKS.getValues()
                .stream()
                .filter(block -> block instanceof FenceBlock)
                .filter(block -> block instanceof DiagonalBlock diag && diag.hasProperties())
                .flatMap(block -> block.getStateDefinition().getPossibleStates().stream())
                .forEach(state -> {

                    ResourceLocation fenceLocation = BlockModelShaper.stateToModelLocation(state);
                    BakedModel model = registry.get(fenceLocation);

                    if (model instanceof MultiPartBakedModel fenceModel) {

                        MultiPartBakedModel newModel = appendDiagonalFenceSelectors(state.getBlock(), fenceModel);
                        registry.put(fenceLocation, newModel);
                    }
                    else if (!erroredBlocks.contains(state.getBlock())){

                        erroredBlocks.add(state.getBlock());
                        DiagonalFences.LOGGER.info(String.format(
                                "Fence block '%s' is not using multipart models, diagonal fence connections may not be visible!",
                                state.getBlock()
                        ));
                    }
                });

        ForgeRegistries.BLOCKS.getValues()
                .stream()
                .filter(block -> block instanceof IronBarsBlock)
                .filter(block -> block instanceof DiagonalBlock diag && diag.hasProperties())
                .flatMap(block -> block.getStateDefinition().getPossibleStates().stream())
                .forEach(state -> {

                    ResourceLocation fenceLocation = BlockModelShaper.stateToModelLocation(state);
                    BakedModel model = registry.get(fenceLocation);

                    if (model instanceof MultiPartBakedModel fenceModel) {

                        MultiPartBakedModel newModel = appendDiagonalPaneSelectors(state.getBlock(), fenceModel);
                        registry.put(fenceLocation, newModel);
                    }
                    else if (!erroredBlocks.contains(state.getBlock())){

                        erroredBlocks.add(state.getBlock());
                        DiagonalFences.LOGGER.info(String.format(
                                "Fence block '%s' is not using multipart models, diagonal fence connections may not be visible!",
                                state.getBlock()
                        ));
                    }
                });
    }

    private static MultiPartBakedModel appendDiagonalFenceSelectors(Block block, MultiPartBakedModel model) {

        List<MultipartDirectionData> oneArmStates = List.of(
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.NORTH, true), state -> state.getValue(DiagonalBlock.NORTH_EAST), Direction.NORTH),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.EAST, true), state -> state.getValue(DiagonalBlock.SOUTH_EAST), Direction.EAST),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.SOUTH, true), state -> state.getValue(DiagonalBlock.SOUTH_WEST), Direction.SOUTH), 
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.WEST, true), state -> state.getValue(DiagonalBlock.NORTH_WEST), Direction.WEST)
        );

        return MultipartAppender.appendDiagonalSelectors(block, oneArmStates, model);
    }

    private static MultiPartBakedModel appendDiagonalPaneSelectors(Block block, MultiPartBakedModel model) {

        List<MultipartDirectionData> oneArmStates = List.of(
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.NORTH, true), state -> state.getValue(DiagonalBlock.NORTH_EAST), Direction.NORTH),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.EAST, true), state -> state.getValue(DiagonalBlock.SOUTH_EAST), Direction.EAST),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.SOUTH, true), state -> state.getValue(DiagonalBlock.SOUTH_WEST), Direction.SOUTH),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.WEST, true), state -> state.getValue(DiagonalBlock.NORTH_WEST), Direction.WEST),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.NORTH, true), state -> !state.getValue(DiagonalBlock.NORTH_EAST), Direction.NORTH),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.EAST, true), state -> !state.getValue(DiagonalBlock.SOUTH_EAST), Direction.EAST),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.SOUTH, true), state -> !state.getValue(DiagonalBlock.SOUTH_WEST), Direction.SOUTH),
                MultipartDirectionData.of(block.defaultBlockState().setValue(FenceBlock.WEST, true), state -> !state.getValue(DiagonalBlock.NORTH_WEST), Direction.WEST)
        );

        return MultipartAppender.appendDiagonalSelectors(block, oneArmStates, model);
    }
}
