package fuzs.diagonalfences.core;

import fuzs.diagonalfences.api.v2.block.DiagonalFenceBlock;
import fuzs.diagonalfences.api.v2.block.DiagonalGlassPaneBlock;
import fuzs.diagonalfences.api.v2.block.DiagonalWallBlock;
import fuzs.diagonalfences.world.level.block.*;
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
