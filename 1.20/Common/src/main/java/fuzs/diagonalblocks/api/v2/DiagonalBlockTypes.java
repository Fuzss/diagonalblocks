package fuzs.diagonalblocks.api.v2;

import fuzs.diagonalblocks.api.v2.impl.DiagonalBlockTypeImpl;
import fuzs.diagonalblocks.core.CommonAbstractions;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.WallBlock;

public final class DiagonalBlockTypes {
    public static final DiagonalBlockType FENCE = new DiagonalBlockTypeImpl("fences", FenceBlock.class, CommonAbstractions.INSTANCE::getDiagonalFenceBlock, () -> Blocks.OAK_FENCE);
    public static final DiagonalBlockType WINDOW = new DiagonalBlockTypeImpl("windows", IronBarsBlock.class, CommonAbstractions.INSTANCE::getDiagonalGlassPaneBlock, () -> Blocks.GLASS_PANE);
    public static final DiagonalBlockType WALL = new DiagonalBlockTypeImpl("walls", WallBlock.class, CommonAbstractions.INSTANCE::getDiagonalWallBlock, () -> Blocks.COBBLESTONE_WALL);

    private DiagonalBlockTypes() {

    }
}
