package fuzs.diagonalfences.client.handler;

import com.google.common.base.Suppliers;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.util.MultipartAppender;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;

import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class FenceModelHandler {
    private static final Supplier<Map<? extends ResourceLocation, Block>> DIAGONAL_BLOCKS = Suppliers.memoize(() -> BuiltInRegistries.BLOCK.stream()
            .filter(block -> block instanceof FenceBlock || block instanceof IronBarsBlock)
            .filter(block -> block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties())
            .flatMap(block -> block.getStateDefinition().getPossibleStates().stream())
            .collect(Collectors.toUnmodifiableMap(BlockModelShaper::stateToModelLocation, BlockBehaviour.BlockStateBase::getBlock)));


    public static EventResultHolder<UnbakedModel> onModifyUnbakedModel(ResourceLocation modelLocation, UnbakedModel unbakedModel, Function<ResourceLocation, UnbakedModel> modelGetter, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        Map<? extends ResourceLocation, Block> diagonalBlocks = DIAGONAL_BLOCKS.get();
        if (diagonalBlocks.containsKey(modelLocation)) {
            Block block = diagonalBlocks.get(modelLocation);
            if (unbakedModel instanceof MultiPart multiPart) {
                MultipartAppender.appendDiagonalSelectors(modelAdder, multiPart, block instanceof IronBarsBlock);
                return EventResultHolder.interrupt(multiPart);
            } else {
                DiagonalFences.LOGGER.warn("Block '{}' is not using multipart model, diagonal connections will not be visible!", block);
            }
        }
        return EventResultHolder.pass();
    }
}
