package fuzs.diagonalfences.data;

import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.puzzleslib.api.data.v2.AbstractLootProvider;
import fuzs.puzzleslib.api.data.v2.core.DataProviderContext;

public class DynamicBlockLootProvider extends AbstractLootProvider.Blocks {

    public DynamicBlockLootProvider(DataProviderContext context) {
        super(context);
    }

    @Override
    public void addLootTables() {
        for (DiagonalBlockType type : DiagonalBlockType.values()) {
            type.getConversions().values().forEach(this::dropSelf);
        }
    }
}
