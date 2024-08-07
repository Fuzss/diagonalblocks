package fuzs.diagonalblocks.handler;

import com.google.common.collect.BiMap;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.puzzleslib.api.block.v1.BlockConversionHelper;
import fuzs.puzzleslib.api.event.v1.RegistryEntryAddedCallback;
import fuzs.puzzleslib.api.init.v3.registry.RegistryHelper;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;

import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class DiagonalBlockHandler {

    public static RegistryEntryAddedCallback<Block> onBlockAdded(DiagonalBlockType type) {
        return (Registry<Block> registry, ResourceLocation id, Block entry, BiConsumer<ResourceLocation, Supplier<Block>> registrar) -> {
            if (type.isTarget(id, entry)) {
                ResourceLocation resourceLocation = type.id(id.getNamespace() + "/" + id.getPath());
                registrar.accept(resourceLocation, () -> {
                    return type.makeDiagonalBlock(id, entry);
                });
            }
        };
    }

    public static void onTagsUpdated(RegistryAccess registryAccess, boolean client) {
        for (Map.Entry<ResourceKey<Item>, Item> entry : BuiltInRegistries.ITEM.entrySet()) {
            if (entry.getValue() instanceof BlockItem blockItem) {
                Block block = blockItem.getBlock();
                setItemForBlock(entry.getKey().location(), blockItem, block);
                setBlockForItem(blockItem, block);
            }
        }
        if (!client) {
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                type.getBlockConversions().forEach(BlockConversionHelper::copyBoundTags);
            }
        }
    }

    private static void setItemForBlock(ResourceLocation resourceLocation, BlockItem blockItem, Block block) {
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            // item id should be fine to use for block items
            if (type.isTarget(resourceLocation, block)) {
                BlockConversionHelper.setItemForBlock(type.getBlockConversions().get(block), blockItem);
                break;
            }
        }
    }

    private static void setBlockForItem(BlockItem blockItem, Block block) {
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            BiMap<Block, Block> conversions = type.getBlockConversions();
            Block baseBlock;
            Block diagonalBlock = conversions.get(block);
            if (diagonalBlock != null) {
                baseBlock = block;
            } else {
                baseBlock = conversions.inverse().get(block);
                if (baseBlock != null) {
                    diagonalBlock = block;
                } else {
                    continue;
                }
            }
            if (RegistryHelper.is(type.getBlacklistTagKey(), baseBlock)) {
                BlockConversionHelper.setBlockForItem(blockItem, baseBlock);
            } else {
                BlockConversionHelper.setBlockForItem(blockItem, diagonalBlock);
            }
            break;
        }
    }
}
