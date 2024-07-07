package fuzs.diagonalblocks.neoforge.api.v2.impl;

import fuzs.diagonalblocks.api.v2.impl.DiagonalFenceBlock;
import fuzs.diagonalblocks.neoforge.client.extensions.DiagonalClientBlockExtensions;
import net.minecraft.world.level.block.Block;
import net.neoforged.neoforge.client.extensions.common.IClientBlockExtensions;

import java.util.function.Consumer;

public class NeoForgeDiagonalFenceBlock extends DiagonalFenceBlock {

    public NeoForgeDiagonalFenceBlock(Block block) {
        super(block);
    }

    @Override
    public void initializeClient(Consumer<IClientBlockExtensions> consumer) {
        consumer.accept(new DiagonalClientBlockExtensions());
    }
}
