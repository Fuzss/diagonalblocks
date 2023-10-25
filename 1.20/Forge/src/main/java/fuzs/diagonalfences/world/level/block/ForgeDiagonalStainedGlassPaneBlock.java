package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.api.v2.block.DiagonalStainedGlassPaneBlock;
import fuzs.diagonalfences.client.extensions.DiagonalClientBlockExtensions;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.extensions.common.IClientBlockExtensions;

import java.util.function.Consumer;

public class ForgeDiagonalStainedGlassPaneBlock extends DiagonalStainedGlassPaneBlock {

    public ForgeDiagonalStainedGlassPaneBlock(Block block, DyeColor color) {
        super(block, color);
    }

    @Override
    public void initializeClient(Consumer<IClientBlockExtensions> consumer) {
        consumer.accept(new DiagonalClientBlockExtensions());
    }
}
