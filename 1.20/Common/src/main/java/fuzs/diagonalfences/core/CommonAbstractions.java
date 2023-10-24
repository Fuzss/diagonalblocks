package fuzs.diagonalfences.core;

import fuzs.diagonalfences.world.level.block.DiagonalFenceBlock;
import fuzs.diagonalfences.world.level.block.DiagonalGlassPaneBlock;
import fuzs.diagonalfences.world.level.block.DiagonalWallBlock;
import fuzs.puzzleslib.api.core.v1.ServiceProviderHelper;
import net.minecraft.world.level.block.Block;

public interface CommonAbstractions {
    CommonAbstractions INSTANCE = ServiceProviderHelper.load(CommonAbstractions.class);

    DiagonalFenceBlock getDiagonalFenceBlock(Block block);

    DiagonalGlassPaneBlock getDiagonalGlassPaneBlock(Block block);

    DiagonalWallBlock getDiagonalWallBlock(Block block);
}
