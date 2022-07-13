package fuzs.diagonalfences;

import net.fabricmc.api.ModInitializer;

public class DiagonalFencesFabric implements ModInitializer {

    @Override
    public void onInitialize() {
        DiagonalFences.onConstructMod();
    }
}
