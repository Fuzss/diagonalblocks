package fuzs.diagonalfences;

import fuzs.diagonalfences.init.ModRegistry;
import fuzs.puzzleslib.core.ModConstructor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DiagonalFences implements ModConstructor {
    public static final String MOD_ID = "diagonalfences";
    public static final String MOD_NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.MOD_NAME);

    @Override
    public void onConstructMod() {
        ModRegistry.touch();
    }
}
