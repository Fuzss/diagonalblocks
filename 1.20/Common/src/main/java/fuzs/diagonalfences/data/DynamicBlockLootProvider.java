package fuzs.diagonalfences.data;

import fuzs.diagonalfences.handler.WallBlockHandler;
import fuzs.puzzleslib.api.data.v2.AbstractLootProvider;
import fuzs.puzzleslib.api.data.v2.core.DataProviderContext;

public class DynamicBlockLootProvider extends AbstractLootProvider.Blocks {

    public DynamicBlockLootProvider(DataProviderContext context) {
        super(context);
    }

    @Override
    public void addLootTables() {
        WallBlockHandler.getWallBlocks().values().forEach(this::dropSelf);
    }
}
