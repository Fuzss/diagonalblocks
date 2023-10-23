package fuzs.diagonalfences.client.handler;

import com.google.common.base.Suppliers;
import com.google.common.collect.Sets;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.client.util.MultipartAppender;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockBehaviour;

import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class FenceModelHandler {
    private static final Supplier<Map<? extends ResourceLocation, Block>> DIAGONAL_BLOCKS = Suppliers.memoize(() -> BuiltInRegistries.BLOCK.stream()
            .filter(block -> block instanceof DiagonalBlock diagonalBlock && (diagonalBlock.getType() == DiagonalBlockType.FENCES || diagonalBlock.getType() == DiagonalBlockType.WINDOWS))
            .flatMap(block -> block.getStateDefinition().getPossibleStates().stream())
            .collect(Collectors.toUnmodifiableMap(BlockModelShaper::stateToModelLocation, BlockBehaviour.BlockStateBase::getBlock)));
    private static final Set<Block> REPORTED_BLOCKS = Sets.newIdentityHashSet();

    public static EventResultHolder<UnbakedModel> onModifyUnbakedModel(ResourceLocation modelLocation, UnbakedModel unbakedModel, Function<ResourceLocation, UnbakedModel> modelGetter, BiConsumer<ResourceLocation, UnbakedModel> modelAdder) {
        Map<? extends ResourceLocation, Block> diagonalBlocks = DIAGONAL_BLOCKS.get();
        if (diagonalBlocks.containsKey(modelLocation)) {
            Block block = diagonalBlocks.get(modelLocation);
            if (unbakedModel instanceof MultiPart multiPart) {
                multiPart = MultipartAppender.appendDiagonalSelectors(modelAdder, multiPart, ((DiagonalBlock) block).getType() == DiagonalBlockType.WINDOWS);
                return EventResultHolder.interrupt(multiPart);
            } else if (REPORTED_BLOCKS.add(block)) {
                DiagonalFences.LOGGER.warn("Block '{}' is not using multipart model, diagonal connections will not be visible!", block);
            }
        }
        return EventResultHolder.pass();
    }
}
