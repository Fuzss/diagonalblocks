package fuzs.diagonalfences.element;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.client.element.DiagonalFencesExtension;
import fuzs.puzzleslib.element.extension.ClientExtensibleElement;
import net.minecraft.block.Block;
import net.minecraft.tags.BlockTags;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.Tags;

public class DiagonalFencesElement extends ClientExtensibleElement<DiagonalFencesExtension> {

    public static final Tags.IOptionalNamedTag<Block> NON_DIAGONAL_FENCES_TAG = BlockTags.createOptional(new ResourceLocation(DiagonalFences.MODID, "non_diagonal_fences"));

    public DiagonalFencesElement() {

        super(element -> new DiagonalFencesExtension((DiagonalFencesElement) element));
    }

    @Override
    public String[] getDescription() {

        return new String[]{"Fences connecting diagonally? Wait. That's illegal."};
    }

}
