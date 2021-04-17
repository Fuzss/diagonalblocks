package com.fuzs.diagonalfences.element;

import com.fuzs.diagonalfences.DiagonalFences;
import com.fuzs.diagonalfences.client.element.DiagonalFencesExtension;
import com.fuzs.diagonalfences.client.element.DiagonalWindowsExtension;
import com.fuzs.puzzleslib_df.element.extension.ClientExtensibleElement;
import net.minecraft.block.Block;
import net.minecraft.tags.BlockTags;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.Tags;

public class DiagonalWindowsElement extends ClientExtensibleElement<DiagonalWindowsExtension> {

    public static final Tags.IOptionalNamedTag<Block> NON_DIAGONAL_PANES_TAG = BlockTags.createOptional(new ResourceLocation(DiagonalFences.MODID, "non_diagonal_panes"));

    public DiagonalWindowsElement() {

        super(element -> new DiagonalWindowsExtension((DiagonalWindowsElement) element));
    }

    @Override
    public String[] getDescription() {

        return new String[]{"Diagonal panes? Is this even allowed?"};
    }

}
