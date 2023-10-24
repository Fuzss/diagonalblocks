package fuzs.diagonalfences.api.v2;

import com.google.common.collect.BiMap;
import com.google.common.collect.Sets;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;

public interface DiagonalBlockType {
    Set<DiagonalBlockType> TYPES = Sets.newConcurrentHashSet();

    static void register(DiagonalBlockType diagonalBlockType) {
        if (TYPES.contains(diagonalBlockType) || !TYPES.add(diagonalBlockType)) {
            throw new IllegalStateException("duplicate diagonal block type '%s'".formatted(diagonalBlockType));
        }
    }

    TagKey<Block> getBlacklistTagKey();

    boolean isTarget(Block block);

    Block makeDiagonalBlock(ResourceLocation resourceLocation, Block block);

    BiMap<Block, Block> getConversions();

    Map<BlockState, BlockState> getBlockStateConversions();

    /**
     * Allows for providing a custom diagonal block factory that will be used for the specified resource location instead of the default diagonal block implementation supplied by this mod.
     *
     * @param resourceLocation the identifier for the block to override the factory for (this is the identifier for the normal block, not for the diagonal block!)
     * @param factory          the factory override
     */
    void registerBlockFactory(ResourceLocation resourceLocation, UnaryOperator<Block> factory);
}
