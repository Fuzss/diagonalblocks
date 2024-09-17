package fuzs.diagonalblocks.core;

import fuzs.diagonalblocks.api.v2.impl.DiagonalFenceBlock;
import fuzs.diagonalblocks.api.v2.impl.DiagonalGlassPaneBlock;
import fuzs.diagonalblocks.api.v2.impl.DiagonalWallBlock;
import fuzs.puzzleslib.api.core.v1.ServiceProviderHelper;
import net.minecraft.world.level.block.Block;

public interface CommonAbstractions {
    CommonAbstractions INSTANCE = ServiceProviderHelper.load(CommonAbstractions.class);

    DiagonalFenceBlock getDiagonalFenceBlock(Block block);

    DiagonalGlassPaneBlock getDiagonalGlassPaneBlock(Block block);

    DiagonalWallBlock getDiagonalWallBlock(Block block);
}
