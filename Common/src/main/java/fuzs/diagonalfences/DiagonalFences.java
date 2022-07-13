package fuzs.diagonalfences;

import fuzs.diagonalfences.init.ModRegistry;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DiagonalFences {
    public static final String MOD_ID = "diagonalfences";
    public static final String MOD_NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.MOD_NAME);

    public static void onConstructMod() {
        ModRegistry.touch();
    }
}
