package fuzs.diagonalblocks.core;

import fuzs.diagonalblocks.api.v2.impl.DiagonalFenceBlock;
import fuzs.diagonalblocks.api.v2.impl.DiagonalGlassPaneBlock;
import fuzs.diagonalblocks.api.v2.impl.DiagonalStainedGlassPaneBlock;
import fuzs.diagonalblocks.api.v2.impl.DiagonalWallBlock;
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
