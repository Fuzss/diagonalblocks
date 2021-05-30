package com.fuzs.diagonalfences.element;

import com.fuzs.diagonalfences.DiagonalFences;
import com.fuzs.diagonalfences.block.IEightWayBlock;
import com.fuzs.diagonalfences.client.element.DiagonalFencesExtension;
import com.fuzs.puzzleslib_df.element.extension.ClientExtensibleElement;
import net.minecraft.block.Block;
import net.minecraft.block.FenceBlock;
import net.minecraft.tags.BlockTags;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.Tags;
import net.minecraftforge.registries.ForgeRegistries;
import virtuoel.statement.api.StateRefresher;

public class DiagonalFencesElement extends ClientExtensibleElement<DiagonalFencesExtension> {

    public static final Tags.IOptionalNamedTag<Block> NON_DIAGONAL_FENCES_TAG = BlockTags.createOptional(new ResourceLocation(DiagonalFences.MODID, "non_diagonal_fences"));

    public DiagonalFencesElement() {

        super(element -> new DiagonalFencesExtension((DiagonalFencesElement) element));
    }

    @Override
    public String[] getDescription() {

        return new String[]{"Fences connecting diagonally? Wait. That's illegal."};
    }

    @Override
    public void loadCommon() {

        for (Block block : ForgeRegistries.BLOCKS) {

            if (block instanceof FenceBlock) {

                StateRefresher.INSTANCE.addBlockProperty(block, IEightWayBlock.NORTH_EAST, Boolean.FALSE);
                StateRefresher.INSTANCE.addBlockProperty(block, IEightWayBlock.SOUTH_EAST, Boolean.FALSE);
                StateRefresher.INSTANCE.addBlockProperty(block, IEightWayBlock.SOUTH_WEST, Boolean.FALSE);
                StateRefresher.INSTANCE.addBlockProperty(block, IEightWayBlock.NORTH_WEST, Boolean.FALSE);
                ((IEightWayBlock) block).updateStatePaletteMap();
            }
        }
    }

}
