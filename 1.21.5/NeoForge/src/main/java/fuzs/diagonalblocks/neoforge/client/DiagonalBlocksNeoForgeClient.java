package fuzs.diagonalblocks.neoforge.client;

import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.client.DiagonalBlocksClient;
import fuzs.diagonalblocks.neoforge.client.extensions.DiagonalClientBlockExtensions;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.renderer.v1.RenderTypeHelper;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.world.level.block.Block;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.EventPriority;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLLoadCompleteEvent;
import net.neoforged.neoforge.client.extensions.common.RegisterClientExtensionsEvent;

import java.util.Map;

@Mod(value = DiagonalBlocks.MOD_ID, dist = Dist.CLIENT)
public class DiagonalBlocksNeoForgeClient {

    public DiagonalBlocksNeoForgeClient(ModContainer modContainer) {
        ClientModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocksClient::new);
        registerLoadingHandlers(modContainer.getEventBus());
    }

    private static void registerLoadingHandlers(IEventBus eventBus) {
        eventBus.addListener((final FMLLoadCompleteEvent evt) -> {
            // run a custom implementation here, the appropriate method in client mod constructor runs together with other mods, so we might miss some entries
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                for (Map.Entry<Block, Block> entry : type.getBlockConversions().entrySet()) {
                    RenderType renderType = RenderTypeHelper.getRenderType(entry.getKey());
                    RenderTypeHelper.registerRenderType(entry.getValue(), renderType);
                }
            }
        });
        eventBus.addListener(EventPriority.LOW, (final RegisterClientExtensionsEvent evt) -> {
            for (DiagonalBlockType diagonalBlockType : DiagonalBlockType.TYPES) {
                for (Block block : diagonalBlockType.getBlockConversions().values()) {
                    if (!evt.isBlockRegistered(block)) {
                        evt.registerBlock(new DiagonalClientBlockExtensions(), block);
                    }
                }
            }
        });
    }
}
