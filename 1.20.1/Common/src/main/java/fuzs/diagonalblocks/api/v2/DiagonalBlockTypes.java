package fuzs.diagonalblocks.api.v2;

import fuzs.diagonalblocks.api.v2.impl.DiagonalBlockTypeImpl;
import fuzs.diagonalblocks.core.CommonAbstractions;
import fuzs.diagonalblocks.data.ModBlockTagsProvider;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.WallBlock;

import java.util.List;

public final class DiagonalBlockTypes {
    public static final DiagonalBlockType FENCE = new DiagonalBlockTypeImpl("fences",
            FenceBlock.class,
            CommonAbstractions.INSTANCE::getDiagonalFenceBlock,
            CrossCollisionBlock.NORTH,
            CrossCollisionBlock.EAST,
            CrossCollisionBlock.WEST,
            CrossCollisionBlock.SOUTH,
            CrossCollisionBlock.WATERLOGGED
    );
    public static final DiagonalBlockType WINDOW = new DiagonalBlockTypeImpl("windows",
            IronBarsBlock.class,
            CommonAbstractions.INSTANCE::getDiagonalGlassPaneBlock,
            CrossCollisionBlock.NORTH,
            CrossCollisionBlock.EAST,
            CrossCollisionBlock.WEST,
            CrossCollisionBlock.SOUTH,
            CrossCollisionBlock.WATERLOGGED
    );
    public static final DiagonalBlockType WALL = new DiagonalBlockTypeImpl("walls",
            WallBlock.class,
            CommonAbstractions.INSTANCE::getDiagonalWallBlock,
            WallBlock.UP,
            WallBlock.NORTH_WALL,
            WallBlock.EAST_WALL,
            WallBlock.WEST_WALL,
            WallBlock.SOUTH_WALL,
            WallBlock.WATERLOGGED
    );

    static {
        ModBlockTagsProvider.BUILT_IN_BLACKLISTED_TYPES.forEach((DiagonalBlockType diagonalBlockType, List<String> strings) -> {
            strings.stream().map(ResourceLocation::new).forEach(diagonalBlockType::disableBlockFactory);
        });
    }

    private DiagonalBlockTypes() {

    }
}
