package fuzs.diagonalfences.data;

import fuzs.diagonalfences.api.v2.DiagonalBlockType;
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
        for (DiagonalBlockType type : DiagonalBlockType.values()) {
            if (type.miningTagKey != null) {
                this.tag(type.miningTagKey).add(type.getConversions().values().toArray(Block[]::new));
            }
        }
        this.tag(BlockTags.WALLS).add(DiagonalBlockType.WALLS.getConversions().values().toArray(Block[]::new));
    }
}
