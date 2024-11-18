package fuzs.diagonalblocks.api.v2;

import fuzs.diagonalblocks.api.v2.impl.*;
import fuzs.diagonalblocks.data.ModBlockTagsProvider;
import fuzs.puzzleslib.api.core.v1.utility.ResourceLocationHelper;
import net.minecraft.world.level.block.*;

import java.util.List;

public final class DiagonalBlockTypes {
    public static final DiagonalBlockType FENCE = new DiagonalBlockTypeImpl("fences", FenceBlock.class,
            DiagonalFenceBlock::new, CrossCollisionBlock.NORTH, CrossCollisionBlock.EAST, CrossCollisionBlock.WEST,
            CrossCollisionBlock.SOUTH, CrossCollisionBlock.WATERLOGGED
    );
    public static final DiagonalBlockType WINDOW = new DiagonalBlockTypeImpl("windows", IronBarsBlock.class,
            DiagonalBlockTypes::getDiagonalGlassPaneBlock, CrossCollisionBlock.NORTH, CrossCollisionBlock.EAST,
            CrossCollisionBlock.WEST, CrossCollisionBlock.SOUTH, CrossCollisionBlock.WATERLOGGED
    );
    public static final DiagonalBlockType WALL = new DiagonalBlockTypeImpl("walls", WallBlock.class,
            DiagonalWallBlock::new, WallBlock.UP, WallBlock.NORTH_WALL, WallBlock.EAST_WALL, WallBlock.WEST_WALL,
            WallBlock.SOUTH_WALL, WallBlock.WATERLOGGED
    );

    static {
        ModBlockTagsProvider.BUILT_IN_BLACKLISTED_TYPES.forEach(
                (DiagonalBlockType diagonalBlockType, List<String> strings) -> {
                    strings.stream().map(ResourceLocationHelper::parse).forEach(diagonalBlockType::disableBlockFactory);
                });
    }

    private DiagonalBlockTypes() {
        // NO-OP
    }

    static DiagonalGlassPaneBlock getDiagonalGlassPaneBlock(Block block) {
        return block instanceof BeaconBeamBlock beaconBeamBlock ? new DiagonalStainedGlassPaneBlock(block,
                beaconBeamBlock.getColor()
        ) : new DiagonalGlassPaneBlock(block);
    }
}
