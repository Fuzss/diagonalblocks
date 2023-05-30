package fuzs.diagonalfences.init;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.puzzleslib.api.init.v2.RegistryManager;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;

public class ModRegistry {
    static final RegistryManager REGISTRY = RegistryManager.instant(DiagonalFences.MOD_ID);
    public static final TagKey<Block> NON_DIAGONAL_FENCES_BLOCK_TAG = REGISTRY.registerBlockTag("non_diagonal_fences");
    public static final TagKey<Block> NON_DIAGONAL_PANES_BLOCK_TAG = REGISTRY.registerBlockTag("non_diagonal_panes");

    public static void touch() {

    }
}
