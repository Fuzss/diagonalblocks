package fuzs.diagonalfences.handler;

import com.google.common.collect.BiMap;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.puzzleslib.api.block.v1.BlockConversionHelper;
import fuzs.puzzleslib.api.init.v3.RegistryHelper;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;

import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class DiagonalBlockHandler {

    public static void onBlockAdded(Registry<Block> registry, ResourceLocation id, Block entry, BiConsumer<ResourceLocation, Supplier<Block>> registrar) {
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            if (type.isTarget(id, entry)) {
                ResourceLocation resourceLocation = DiagonalFences.id(id.getNamespace() + "/" + id.getPath());
                registrar.accept(resourceLocation, () -> {
                    return type.makeDiagonalBlock(id, entry);
                });
                break;
            }
        }
    }

    public static void onItemAdded(Registry<Item> registry, ResourceLocation id, Item entry, BiConsumer<ResourceLocation, Supplier<Item>> registrar) {
        if (entry instanceof BlockItem blockItem) {
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                Block block = blockItem.getBlock();
                // item id should be fine to use for block items
                if (type.isTarget(id, block)) {
                    BlockConversionHelper.setBlockItemBlock(blockItem, type.getBlockConversions().get(block));
                    break;
                }
            }
        }
    }

    public static void onTagsUpdated(RegistryAccess registryAccess, boolean client) {
        for (Item item : BuiltInRegistries.ITEM) {
            if (item instanceof BlockItem blockItem) {
                Block block = blockItem.getBlock();
                for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                    BiMap<Block, Block> conversions = type.getBlockConversions();
                    Block base;
                    Block diagonal = conversions.get(block);
                    if (diagonal != null) {
                        base = block;
                    } else {
                        base = conversions.inverse().get(block);
                    }
                    if (base != null) {
                        diagonal = block;
                    } else {
                        continue;
                    }
                    if (RegistryHelper.is(type.getBlacklistTagKey(), base)) {
                        BlockConversionHelper.setBlockForItem(blockItem, base);
                    } else {
                        BlockConversionHelper.setBlockForItem(blockItem, diagonal);
                    }
                    break;
                }
            }
        }
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            type.getBlockConversions().forEach(BlockConversionHelper::copyBoundTags);
        }
    }
}
