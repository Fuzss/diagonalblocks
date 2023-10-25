package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.api.v2.block.DiagonalWallBlock;
import fuzs.diagonalfences.client.extensions.DiagonalClientBlockExtensions;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.extensions.common.IClientBlockExtensions;

import java.util.function.Consumer;

public class ForgeDiagonalWallBlock extends DiagonalWallBlock {

    public ForgeDiagonalWallBlock(Block block) {
        super(block);
    }

    @Override
    public void initializeClient(Consumer<IClientBlockExtensions> consumer) {
        consumer.accept(new DiagonalClientBlockExtensions());
    }
}
