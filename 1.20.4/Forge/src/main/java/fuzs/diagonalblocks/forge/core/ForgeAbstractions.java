package fuzs.diagonalblocks.forge.core;

import fuzs.diagonalblocks.api.v2.impl.*;
import fuzs.diagonalblocks.core.CommonAbstractions;
import fuzs.diagonalblocks.forge.api.v2.impl.ForgeDiagonalFenceBlock;
import fuzs.diagonalblocks.forge.api.v2.impl.ForgeDiagonalGlassPaneBlock;
import fuzs.diagonalblocks.forge.api.v2.impl.ForgeDiagonalStainedGlassPaneBlock;
import fuzs.diagonalblocks.forge.api.v2.impl.ForgeDiagonalWallBlock;
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
