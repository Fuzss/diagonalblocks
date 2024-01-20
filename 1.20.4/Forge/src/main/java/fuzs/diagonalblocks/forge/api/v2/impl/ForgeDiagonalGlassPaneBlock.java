package fuzs.diagonalblocks.forge.api.v2.impl;

import fuzs.diagonalblocks.api.v2.impl.DiagonalGlassPaneBlock;
import fuzs.diagonalblocks.forge.client.extensions.DiagonalClientBlockExtensions;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.extensions.common.IClientBlockExtensions;

import java.util.function.Consumer;

public class ForgeDiagonalGlassPaneBlock extends DiagonalGlassPaneBlock {

    public ForgeDiagonalGlassPaneBlock(Block block) {
        super(block);
    }

    @Override
    public void initializeClient(Consumer<IClientBlockExtensions> consumer) {
        consumer.accept(new DiagonalClientBlockExtensions());
    }
}
