package fuzs.diagonalblocks.neoforge.api.v2.impl;

import fuzs.diagonalblocks.api.v2.impl.DiagonalStainedGlassPaneBlock;
import fuzs.diagonalblocks.neoforge.client.extensions.DiagonalClientBlockExtensions;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.level.block.Block;
import net.neoforged.neoforge.client.extensions.common.IClientBlockExtensions;

import java.util.function.Consumer;

public class NeoForgeDiagonalStainedGlassPaneBlock extends DiagonalStainedGlassPaneBlock {

    public NeoForgeDiagonalStainedGlassPaneBlock(Block block, DyeColor color) {
        super(block, color);
    }

    @Override
    public void initializeClient(Consumer<IClientBlockExtensions> consumer) {
        consumer.accept(new DiagonalClientBlockExtensions());
    }
}
