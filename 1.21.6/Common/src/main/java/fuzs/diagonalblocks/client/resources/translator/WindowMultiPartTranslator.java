package fuzs.diagonalblocks.client.resources.translator;

import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.resources.model.MultiPartAppender;
import net.minecraft.client.renderer.block.model.BlockModelDefinition;

public final class WindowMultiPartTranslator extends MultiPartTranslator {

    public WindowMultiPartTranslator() {
        super(DiagonalBlockTypes.WINDOW);
    }

    @Override
    protected BlockModelDefinition.MultiPartDefinition applyAdditionalSelectors(BlockModelDefinition.MultiPartDefinition multiPart) {
        return MultiPartAppender.appendDiagonalSelectors(multiPart, true);
    }
}
