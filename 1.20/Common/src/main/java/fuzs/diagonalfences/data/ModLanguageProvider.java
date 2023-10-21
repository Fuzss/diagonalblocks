package fuzs.diagonalfences.data;

import fuzs.diagonalfences.world.level.block.DiagonalWallBlock;
import fuzs.puzzleslib.api.client.data.v2.AbstractLanguageProvider;
import fuzs.puzzleslib.api.data.v2.core.DataProviderContext;

public class ModLanguageProvider extends AbstractLanguageProvider {

    public ModLanguageProvider(DataProviderContext context) {
        super(context);
    }

    @Override
    protected void addTranslations(TranslationBuilder builder) {
        builder.add(DiagonalWallBlock.DIAGONAL_TRANSLATION_KEY, "Diagonal %s");
    }
}
