package fuzs.diagonalblocks.neoforge.integration.cfm;

import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.DyeColor;

public class MrCrayfishFurnitureMod {
    private static final String[] WOOD_TYPES = {"oak", "birch", "spruce", "jungle", "acacia", "dark_oak", "crimson", "warped", "mangrove", "bamboo", "cherry"};

    public static void init() {
        for (String woodType : WOOD_TYPES) {
            DiagonalBlockTypes.FENCE.registerDefaultBlockFactory(id(woodType + "_upgraded_fence"));
            DiagonalBlockTypes.FENCE.registerDefaultBlockFactory(id("stripped_" + woodType + "_upgraded_fence"));
        }
        for (DyeColor dyeColor : DyeColor.values()) {
            DiagonalBlockTypes.FENCE.registerDefaultBlockFactory(id(dyeColor + "_picket_fence"));
        }
    }

    public static ResourceLocation id(String path) {
        return new ResourceLocation("cfm", path);
    }
}
