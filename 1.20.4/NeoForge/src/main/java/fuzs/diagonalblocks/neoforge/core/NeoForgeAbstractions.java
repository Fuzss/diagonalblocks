package fuzs.diagonalblocks.neoforge.core;

import fuzs.diagonalblocks.api.v2.impl.*;
import fuzs.diagonalblocks.core.CommonAbstractions;
import fuzs.diagonalblocks.neoforge.api.v2.impl.NeoForgeDiagonalFenceBlock;
import fuzs.diagonalblocks.neoforge.api.v2.impl.NeoForgeDiagonalGlassPaneBlock;
import fuzs.diagonalblocks.neoforge.api.v2.impl.NeoForgeDiagonalStainedGlassPaneBlock;
import fuzs.diagonalblocks.neoforge.api.v2.impl.NeoForgeDiagonalWallBlock;
import net.minecraft.world.level.block.BeaconBeamBlock;
import net.minecraft.world.level.block.Block;

public class NeoForgeAbstractions implements CommonAbstractions {

    @Override
    public DiagonalFenceBlock getDiagonalFenceBlock(Block block) {
        return new NeoForgeDiagonalFenceBlock(block);
    }

    @Override
    public DiagonalGlassPaneBlock getDiagonalGlassPaneBlock(Block block) {
        return block instanceof BeaconBeamBlock beaconBeamBlock ? new NeoForgeDiagonalStainedGlassPaneBlock(block, beaconBeamBlock.getColor()) : new NeoForgeDiagonalGlassPaneBlock(block);
    }

    @Override
    public DiagonalWallBlock getDiagonalWallBlock(Block block) {
        return new NeoForgeDiagonalWallBlock(block);
    }
}
