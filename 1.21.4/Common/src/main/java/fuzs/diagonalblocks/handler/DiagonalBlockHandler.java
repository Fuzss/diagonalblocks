package fuzs.diagonalblocks.handler;

import com.google.common.collect.BiMap;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.puzzleslib.api.block.v1.BlockConversionHelper;
import fuzs.puzzleslib.api.event.v1.RegistryEntryAddedCallback;
import fuzs.puzzleslib.api.event.v1.core.EventResultHolder;
import fuzs.puzzleslib.api.init.v3.registry.RegistryHelper;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.LevelEvent;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.gameevent.GameEvent;
import net.minecraft.world.phys.BlockHitResult;

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

    public static EventResultHolder<InteractionResult> onUseBlock(Player player, Level level, InteractionHand interactionHand, BlockHitResult hitResult) {
        // allow for toggling between diagonal and non-diagonal block variants via shift+right-clicking with empty hand
        if (player.isSecondaryUseActive() && player.getItemInHand(interactionHand).isEmpty()) {
            BlockPos blockPos = hitResult.getBlockPos();
            BlockState blockState = level.getBlockState(blockPos);
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                Block newBlock;
                Block block = blockState.getBlock();
                if (type.getBlockConversions().containsKey(block)) {
                    newBlock = type.getBlockConversions().get(block);
                } else if (type.getBlockConversions().containsValue(block)) {
                    newBlock = type.getBlockConversions().inverse().get(block);
                } else {
                    newBlock = null;
                }
                if (newBlock != null) {
                    BlockState newBlockState = newBlock.withPropertiesOf(blockState);
                    newBlockState = Block.updateFromNeighbourShapes(newBlockState, level, blockPos);
                    level.setBlock(blockPos, newBlockState, 3);
                    level.neighborChanged(blockPos, newBlock, null);
                    level.gameEvent(GameEvent.BLOCK_CHANGE, blockPos, GameEvent.Context.of(player, newBlockState));
                    level.levelEvent(player, LevelEvent.PARTICLES_AND_SOUND_WAX_ON, blockPos, 0);
                    return EventResultHolder.interrupt(InteractionResult.SUCCESS);
                }
            }
        }

        return EventResultHolder.pass();
    }

    public static void onTagsUpdated(HolderLookup.Provider registries, boolean client) {
        for (Map.Entry<ResourceKey<Item>, Item> entry : BuiltInRegistries.ITEM.entrySet()) {
            if (entry.getValue() instanceof BlockItem blockItem) {
                Block block = blockItem.getBlock();
                setItemForBlock(entry.getKey().location(), blockItem, block);
                setBlockForItem(blockItem, block);
            }
        }
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            type.getBlockConversions().forEach(BlockConversionHelper::copyBoundTags);
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
