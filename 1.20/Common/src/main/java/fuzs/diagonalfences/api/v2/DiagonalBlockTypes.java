package fuzs.diagonalfences.api.v2;

import fuzs.diagonalfences.core.CommonAbstractions;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.WallBlock;

public final class DiagonalBlockTypes {
    public static final DiagonalBlockType FENCE = new DiagonalBlockTypeImpl("fences", FenceBlock.class, CommonAbstractions.INSTANCE::getDiagonalFenceBlock);
    public static final DiagonalBlockType WINDOW = new DiagonalBlockTypeImpl("windows", IronBarsBlock.class, CommonAbstractions.INSTANCE::getDiagonalGlassPaneBlock);
    public static final DiagonalBlockType WALL = new DiagonalBlockTypeImpl("walls", WallBlock.class, CommonAbstractions.INSTANCE::getDiagonalWallBlock);

    private DiagonalBlockTypes() {

    }
}
