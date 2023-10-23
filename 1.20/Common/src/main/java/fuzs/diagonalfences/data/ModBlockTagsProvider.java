package fuzs.diagonalfences.data;

import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.puzzleslib.api.data.v2.AbstractTagProvider;
import fuzs.puzzleslib.api.data.v2.core.DataProviderContext;
import net.minecraft.core.HolderLookup;

public class ModBlockTagsProvider extends AbstractTagProvider.Blocks {

    public ModBlockTagsProvider(DataProviderContext context) {
        super(context);
    }

    @Override
    protected void addTags(HolderLookup.Provider provider) {
        for (DiagonalBlockType type : DiagonalBlockType.values()) {
            this.tag(type.blacklistTagKey);
        }
    }
}
