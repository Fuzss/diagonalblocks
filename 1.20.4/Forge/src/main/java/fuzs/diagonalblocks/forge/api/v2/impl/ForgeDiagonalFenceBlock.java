package fuzs.diagonalblocks.forge.api.v2.impl;

import fuzs.diagonalblocks.api.v2.impl.DiagonalFenceBlock;
import fuzs.diagonalblocks.forge.client.extensions.DiagonalClientBlockExtensions;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.extensions.common.IClientBlockExtensions;

import java.util.function.Consumer;

public class ForgeDiagonalFenceBlock extends DiagonalFenceBlock {

    public ForgeDiagonalFenceBlock(Block block) {
        super(block);
    }

    @Override
    public void initializeClient(Consumer<IClientBlockExtensions> consumer) {
        consumer.accept(new DiagonalClientBlockExtensions());
    }
}
