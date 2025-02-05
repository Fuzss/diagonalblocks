package fuzs.diagonalblocks.client.resources.translator;

import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.resources.model.MultipartAppender;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;

import java.util.function.BiConsumer;

public class WindowMultiPartTranslator extends MultiPartTranslator {

    public WindowMultiPartTranslator() {
        super(DiagonalBlockTypes.WINDOW);
    }

    @Override
    protected MultiPart applyAdditionalSelectors(BiConsumer<ResourceLocation, UnbakedModel> modelAdder, MultiPart multiPart) {
        return MultipartAppender.appendDiagonalSelectors(modelAdder, multiPart, true);
    }
}
