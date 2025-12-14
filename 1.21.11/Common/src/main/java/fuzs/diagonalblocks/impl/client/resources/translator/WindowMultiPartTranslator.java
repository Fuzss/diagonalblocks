package fuzs.diagonalblocks.impl.client.resources.translator;

import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.impl.client.resources.model.MultiPartAppender;
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
