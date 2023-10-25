package fuzs.diagonalfences.api.v2;

import com.google.common.collect.BiMap;
import com.google.common.collect.Sets;
import fuzs.diagonalfences.handler.DiagonalBlockHandler;
import fuzs.puzzleslib.api.event.v1.RegistryEntryAddedCallback;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;

import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * A provider for logic surrounding a type of diagonal block like fence or wall.
 */
public interface DiagonalBlockType {
    /**
     * All registered diagonal block types.
     */
    Set<DiagonalBlockType> TYPES = Sets.newConcurrentHashSet();

    /**
     * Registers a new diagonal block type, checking for duplicates in the process.
     *
     * @param type the new diagonal block type
     */
    static void register(DiagonalBlockType type) {
        if (TYPES.contains(type) || !TYPES.add(type)) {
            throw new IllegalStateException("duplicate diagonal block type '%s'".formatted(type));
        } else {
            // due to how this event is implemented on Fabric it's registration must be triggered by the mod registering the new diagonal block type
            RegistryEntryAddedCallback.registryEntryAdded(Registries.BLOCK).register(DiagonalBlockHandler.onBlockAdded(type));
        }
    }

    /**
     * Creates a new {@link ResourceLocation} for this type with the type's namespace.
     *
     * @param path path for the identifier
     * @return the new identifier for the type
     */
    ResourceLocation id(String path);

    /**
     * A block blacklist for preventing the diagonal block from being set to the corresponding {@link net.minecraft.world.item.BlockItem}.
     * <p>Note that the diagonal block will still exist (although unused). It is registered to the block registry and has a block model generated for it.
     * Preventing block registration is possible using {@link #disableBlockFactory(ResourceLocation)}.
     *
     * @return the blacklist block tag key
     */
    TagKey<Block> getBlacklistTagKey();

    /**
     * Can a block be targeted by this type and therefore receive a diagonal variant replacing the original block.
     *
     * @param resourceLocation identifier for the block
     * @param block            the block
     * @return can this block receive a diagonal variant
     */
    boolean isTarget(ResourceLocation resourceLocation, Block block);

    /**
     * Creates a new diagonal block implementation from the non-diagonal base block.
     * <p>The result of this method in the default implementation can be further customized via {@link #registerBlockFactory(ResourceLocation, UnaryOperator)},
     * {@link #registerDefaultBlockFactory(ResourceLocation)} and {@link #disableBlockFactory(ResourceLocation)}.
     *
     * @param resourceLocation the identifier for the non-diagonal base block
     * @param block            the non-diagonal base block
     * @return the new diagonal block replacement
     */
    Block makeDiagonalBlock(ResourceLocation resourceLocation, Block block);

    /**
     * A map containing pairs of the original non-diagonal block as keys and the diagonal block equivalents as corresponding values.
     *
     * @return non-diagonal to diagonal blocks identity map
     */
    BiMap<Block, Block> getBlockConversions();

    /**
     * Allows for providing a custom diagonal block factory that will be used for the specified resource location instead of the default diagonal block implementation supplied by this mod.
     * <p>Useful if a mod extends the vanilla base class (like {@link net.minecraft.world.level.block.FenceBlock}) with some custom logic which is otherwise lost in the diagonal block.
     *
     * @param resourceLocation the identifier for the block to override the factory for (this is the identifier for the non-diagonal base block, not for the diagonal block)
     * @param factory          the factory override
     */
    void registerBlockFactory(ResourceLocation resourceLocation, UnaryOperator<Block> factory);

    /**
     * Allows for forcing a block to be turned into a diagonal block, even though it would otherwise not be targeted (see {@link #isTarget(ResourceLocation, Block)}).
     * <p>Useful for blocks that do not extend the vanilla base class (like {@link net.minecraft.world.level.block.FenceBlock}), but still provide the same functionality.
     *
     * @param resourceLocation the identifier for the block to override the factory for (this is the identifier for the non-diagonal base block, not for the diagonal block)
     */
    void registerDefaultBlockFactory(ResourceLocation resourceLocation);

    /**
     * Prevents a block from generating a diagonal variant. Provides an alternative to the blacklist tag from {@link #getBlacklistTagKey()}.
     * <p>While the blacklist tag only prevents placing new diagonal blocks, but still allows the diagonal block to be registered and exist,
     * disabling a block on this level prevents it from being created at all.
     *
     * @param resourceLocation the identifier for the block to override the factory for (this is the identifier for the non-diagonal base block, not for the diagonal block)
     */
    void disableBlockFactory(ResourceLocation resourceLocation);
}
