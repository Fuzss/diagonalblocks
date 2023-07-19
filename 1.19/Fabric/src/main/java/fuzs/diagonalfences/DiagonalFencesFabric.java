package fuzs.diagonalfences;

import fuzs.puzzleslib.core.CoreServices;
import net.fabricmc.api.ModInitializer;

public class DiagonalFencesFabric implements ModInitializer {

    @Override
    public void onInitialize() {
        CoreServices.FACTORIES.modConstructor(DiagonalFences.MOD_ID).accept(new DiagonalFences());
    }
}
