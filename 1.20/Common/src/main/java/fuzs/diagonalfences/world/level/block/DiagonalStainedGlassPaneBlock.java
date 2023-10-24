package fuzs.diagonalfences.world.level.block;

import net.minecraft.world.item.DyeColor;
import net.minecraft.world.level.block.BeaconBeamBlock;
import net.minecraft.world.level.block.Block;

public class DiagonalStainedGlassPaneBlock extends DiagonalGlassPaneBlock implements BeaconBeamBlock {
    private final DyeColor color;

    public DiagonalStainedGlassPaneBlock(Block block, DyeColor color) {
        super(block);
        this.color = color;
    }

    @Override
    public DyeColor getColor() {
        return this.color;
    }
}
