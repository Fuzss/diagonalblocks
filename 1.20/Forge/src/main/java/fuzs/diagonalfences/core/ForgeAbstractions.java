package fuzs.diagonalfences.core;

import fuzs.diagonalfences.world.level.block.DiagonalWallBlock;
import fuzs.diagonalfences.world.level.block.ForgeDiagonalWallBlock;
import net.minecraft.world.level.block.Block;

public class ForgeAbstractions implements CommonAbstractions {

    @Override
    public DiagonalWallBlock getDiagonalWallBlock(Block block) {
        return new ForgeDiagonalWallBlock(block);
    }
}
