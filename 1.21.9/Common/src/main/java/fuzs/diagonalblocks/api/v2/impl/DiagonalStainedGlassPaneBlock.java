package fuzs.diagonalblocks.api.v2.impl;

import net.minecraft.world.item.DyeColor;
import net.minecraft.world.level.block.BeaconBeamBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;

public class DiagonalStainedGlassPaneBlock extends DiagonalGlassPaneBlock implements BeaconBeamBlock {
    private final DyeColor color;

    public DiagonalStainedGlassPaneBlock(BlockBehaviour.Properties properties, DyeColor color) {
        super(properties);
        this.color = color;
    }

    @Override
    public DyeColor getColor() {
        return this.color;
    }
}
