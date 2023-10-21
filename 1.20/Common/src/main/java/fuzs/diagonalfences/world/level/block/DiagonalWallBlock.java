package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.DiagonalFences;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.WallBlock;

public class DiagonalWallBlock extends LegacyWallBlock {
    public static final String DIAGONAL_TRANSLATION_KEY = "block." + DiagonalFences.MOD_ID + ".diagonal";

    private final WallBlock wallBlock;

    public DiagonalWallBlock(WallBlock wallBlock, Properties properties) {
        super(properties);
        this.wallBlock = wallBlock;
    }

    @Override
    public String getDescriptionId() {
        return this.wallBlock.getDescriptionId();
    }

    @Override
    public MutableComponent getName() {
        return Component.translatable(DIAGONAL_TRANSLATION_KEY, this.getDescriptionId());
    }

    @Override
    public Item asItem() {
        return this.wallBlock.asItem();
    }
}
