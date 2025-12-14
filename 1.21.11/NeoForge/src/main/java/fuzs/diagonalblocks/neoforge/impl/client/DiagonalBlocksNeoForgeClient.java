package fuzs.diagonalblocks.neoforge.impl.client;

import fuzs.diagonalblocks.impl.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockType;
import fuzs.diagonalblocks.impl.client.DiagonalBlocksClient;
import fuzs.diagonalblocks.neoforge.impl.client.extensions.DiagonalClientBlockExtensions;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import net.minecraft.world.level.block.Block;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.EventPriority;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.Mod;
import net.neoforged.neoforge.client.extensions.common.RegisterClientExtensionsEvent;

@Mod(value = DiagonalBlocks.MOD_ID, dist = Dist.CLIENT)
public class DiagonalBlocksNeoForgeClient {

    public DiagonalBlocksNeoForgeClient(ModContainer modContainer) {
        ClientModConstructor.construct(DiagonalBlocks.MOD_ID, DiagonalBlocksClient::new);
        registerLoadingHandlers(modContainer.getEventBus());
    }

    private static void registerLoadingHandlers(IEventBus eventBus) {
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
