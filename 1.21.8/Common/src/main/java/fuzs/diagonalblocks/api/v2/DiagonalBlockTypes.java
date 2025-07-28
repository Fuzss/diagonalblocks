package fuzs.diagonalblocks.api.v2;

import fuzs.diagonalblocks.api.v2.impl.*;
import fuzs.diagonalblocks.data.ModBlockTagsProvider;
import fuzs.puzzleslib.api.core.v1.utility.ResourceLocationHelper;
import net.minecraft.world.level.block.*;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.properties.WallSide;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;

public final class DiagonalBlockTypes {
    public static final DiagonalBlockType FENCE = new DiagonalBlockTypeImpl("fences",
            FenceBlock.class,
            (Block block) -> DiagonalFenceBlock::new,
            CrossCollisionBlock.NORTH,
            CrossCollisionBlock.EAST,
            CrossCollisionBlock.WEST,
            CrossCollisionBlock.SOUTH,
            CrossCollisionBlock.WATERLOGGED);
    public static final DiagonalBlockType WINDOW = new DiagonalBlockTypeImpl("windows",
            IronBarsBlock.class,
            DiagonalBlockTypes::getDiagonalGlassPaneFactory,
            CrossCollisionBlock.NORTH,
            CrossCollisionBlock.EAST,
            CrossCollisionBlock.WEST,
            CrossCollisionBlock.SOUTH,
            CrossCollisionBlock.WATERLOGGED);
    public static final DiagonalBlockType WALL = new DiagonalBlockTypeImpl("walls",
            WallBlock.class,
            (Block block) -> DiagonalWallBlock::new,
            WallBlock.UP,
            WallBlock.NORTH,
            WallBlock.EAST,
            WallBlock.WEST,
            WallBlock.SOUTH,
            WallBlock.WATERLOGGED) {
        @Override
        public boolean supportsOriginalBlockState() {
            // disabled for now, switching between tall and low wall sides does not work correctly when converting to vanilla states
            return false;
        }

        @Override
        protected Property<?> sanitizeProperty(Property<?> property) {
            if (LegacyWallBlock.WALL_SIDE_PROPERTIES.containsKey(property)) {
                return LegacyWallBlock.WALL_SIDE_PROPERTIES.get(property);
            } else {
                return super.sanitizeProperty(property);
            }
        }

        @Override
        protected Comparable<?> sanitizePropertyValue(Property<?> property, Comparable<?> value) {
            if (property.getValueClass() == WallSide.class) {
                return Objects.equals(value, WallSide.NONE) ? Boolean.FALSE : Boolean.TRUE;
            } else {
                return super.sanitizePropertyValue(property, value);
            }
        }
    };

    static {
        ModBlockTagsProvider.BUILT_IN_BLACKLISTED_TYPES.forEach((DiagonalBlockType diagonalBlockType, List<String> strings) -> {
            strings.stream().map(ResourceLocationHelper::parse).forEach(diagonalBlockType::disableBlockFactory);
        });
    }

    private DiagonalBlockTypes() {
        // NO-OP
    }

    static Function<BlockBehaviour.Properties, Block> getDiagonalGlassPaneFactory(Block block) {
        if (block instanceof BeaconBeamBlock beaconBeamBlock) {
            return (BlockBehaviour.Properties properties) -> {
                return new DiagonalStainedGlassPaneBlock(properties, beaconBeamBlock.getColor());
            };
        } else {
            return DiagonalGlassPaneBlock::new;
        }
    }
}
