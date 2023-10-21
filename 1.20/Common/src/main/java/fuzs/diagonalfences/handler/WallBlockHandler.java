package fuzs.diagonalfences.handler;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.mixin.accessor.BlockAccessor;
import fuzs.diagonalfences.mixin.accessor.BlockItemAccessor;
import fuzs.diagonalfences.world.level.block.DiagonalWallBlock;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.WallBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;

import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class WallBlockHandler {
    private static final BiMap<WallBlock, DiagonalWallBlock> WALL_BLOCKS = HashBiMap.create();

    public static void onBlockAdded(ResourceLocation id, Block entry, BiConsumer<ResourceLocation, Supplier<Block>> registrar) {
        if (entry instanceof WallBlock wallBlock) {
            ResourceLocation resourceLocation = DiagonalFences.id(id.getNamespace() + "/" + id.getPath());
            registrar.accept(resourceLocation, () -> {
                DiagonalWallBlock block = new DiagonalWallBlock(wallBlock, BlockBehaviour.Properties.copy(wallBlock));
                WALL_BLOCKS.put(wallBlock, block);
                return block;
            });
        }
    }

    public static void onItemAdded(ResourceLocation id, Item entry, BiConsumer<ResourceLocation, Supplier<Item>> registrar) {
        if (entry instanceof BlockItem blockItem && blockItem.getBlock() instanceof WallBlock wallBlock) {
            setBlockItemBlock(blockItem, WALL_BLOCKS.get(wallBlock));
        }
    }

    private static void setBlockItemBlock(BlockItem item, Block block) {
        Objects.requireNonNull(item, "item is null");
        Objects.requireNonNull(block, "block is null");
        Item.BY_BLOCK.put(block, item);
        ((BlockItemAccessor) item).diagonalwalls$setBlock(block);
        ((BlockAccessor) block).diagonalwalls$setItem(item);
    }

    public static BiMap<WallBlock, DiagonalWallBlock> getWallBlocks() {
        return Maps.unmodifiableBiMap(WALL_BLOCKS);
    }
}
