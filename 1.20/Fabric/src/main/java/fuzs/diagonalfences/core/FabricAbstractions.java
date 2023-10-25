package fuzs.diagonalfences.core;

import fuzs.diagonalfences.api.v2.block.DiagonalFenceBlock;
import fuzs.diagonalfences.api.v2.block.DiagonalGlassPaneBlock;
import fuzs.diagonalfences.api.v2.block.DiagonalStainedGlassPaneBlock;
import fuzs.diagonalfences.api.v2.block.DiagonalWallBlock;
import net.minecraft.world.level.block.BeaconBeamBlock;
import net.minecraft.world.level.block.Block;

public class FabricAbstractions implements CommonAbstractions {

    @Override
    public DiagonalFenceBlock getDiagonalFenceBlock(Block block) {
        return new DiagonalFenceBlock(block);
    }

    @Override
    public DiagonalGlassPaneBlock getDiagonalGlassPaneBlock(Block block) {
        return block instanceof BeaconBeamBlock beaconBeamBlock ? new DiagonalStainedGlassPaneBlock(block, beaconBeamBlock.getColor()) : new DiagonalGlassPaneBlock(block);
    }

    @Override
    public DiagonalWallBlock getDiagonalWallBlock(Block block) {
        return new DiagonalWallBlock(block);
    }
}
