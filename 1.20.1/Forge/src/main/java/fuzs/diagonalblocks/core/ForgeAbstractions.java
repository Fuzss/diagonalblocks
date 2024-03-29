package fuzs.diagonalblocks.core;

import fuzs.diagonalblocks.api.v2.impl.*;
import net.minecraft.world.level.block.BeaconBeamBlock;
import net.minecraft.world.level.block.Block;

public class ForgeAbstractions implements CommonAbstractions {

    @Override
    public DiagonalFenceBlock getDiagonalFenceBlock(Block block) {
        return new ForgeDiagonalFenceBlock(block);
    }

    @Override
    public DiagonalGlassPaneBlock getDiagonalGlassPaneBlock(Block block) {
        return block instanceof BeaconBeamBlock beaconBeamBlock ? new ForgeDiagonalStainedGlassPaneBlock(block, beaconBeamBlock.getColor()) : new ForgeDiagonalGlassPaneBlock(block);
    }

    @Override
    public DiagonalWallBlock getDiagonalWallBlock(Block block) {
        return new ForgeDiagonalWallBlock(block);
    }
}
