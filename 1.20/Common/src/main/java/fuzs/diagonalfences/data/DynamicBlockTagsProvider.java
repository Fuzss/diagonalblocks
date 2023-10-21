package fuzs.diagonalfences.data;

import fuzs.diagonalfences.handler.WallBlockHandler;
import fuzs.puzzleslib.api.data.v2.AbstractTagProvider;
import fuzs.puzzleslib.api.data.v2.core.DataProviderContext;
import net.minecraft.core.HolderLookup;
import net.minecraft.tags.BlockTags;
import net.minecraft.world.level.block.Block;

public class DynamicBlockTagsProvider extends AbstractTagProvider.Blocks {

    public DynamicBlockTagsProvider(DataProviderContext context) {
        super(context);
    }

    @Override
    protected void addTags(HolderLookup.Provider provider) {
        Block[] blocks = WallBlockHandler.getWallBlocks().values().toArray(Block[]::new);
        this.tag(BlockTags.MINEABLE_WITH_PICKAXE).add(blocks);
        this.tag(BlockTags.WALLS).add(blocks);
    }
}
