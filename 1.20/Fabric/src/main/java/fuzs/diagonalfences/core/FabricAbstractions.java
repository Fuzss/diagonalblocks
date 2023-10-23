package fuzs.diagonalfences.core;

import fuzs.diagonalfences.world.level.block.DiagonalWallBlock;
import net.minecraft.world.level.block.Block;

public class FabricAbstractions implements CommonAbstractions {

    @Override
    public DiagonalWallBlock getDiagonalWallBlock(Block block) {
        return new DiagonalWallBlock(block);
    }
}
