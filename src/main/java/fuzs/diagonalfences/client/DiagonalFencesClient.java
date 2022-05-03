package fuzs.diagonalfences.client;

import com.google.common.collect.Sets;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.IDiagonalBlock;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.model.MultipartAppender;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.*;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ModelBakeEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.*;

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
    }

    private static MultiPartBakedModel appendDiagonalFenceSelectors(Block block, MultiPartBakedModel model) {

        Map<BlockState, Direction> oneArmStates = Map.of(
                block.defaultBlockState().setValue(FenceBlock.NORTH, true), Direction.NORTH,
                block.defaultBlockState().setValue(FenceBlock.EAST, true), Direction.EAST,
                block.defaultBlockState().setValue(FenceBlock.SOUTH, true), Direction.SOUTH,
                block.defaultBlockState().setValue(FenceBlock.WEST, true), Direction.WEST
        );

        return MultipartAppender.appendDiagonalSelectors(block, oneArmStates, model);
    }
}
