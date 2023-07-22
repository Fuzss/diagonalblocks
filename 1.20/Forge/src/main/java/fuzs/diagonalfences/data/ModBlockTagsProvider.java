package fuzs.diagonalfences.data;

import fuzs.diagonalfences.init.ModRegistry;
import fuzs.puzzleslib.api.data.v1.AbstractTagProvider;
import net.minecraft.core.HolderLookup;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.data.event.GatherDataEvent;

public class ModBlockTagsProvider extends AbstractTagProvider.Blocks {

    public ModBlockTagsProvider(GatherDataEvent evt, String modId) {
        super(evt, modId);
    }

    @Override
    protected void addTags(HolderLookup.Provider provider) {
        this.tag(ModRegistry.NON_DIAGONAL_FENCES_BLOCK_TAG).addOptional(new ResourceLocation("assorteddecor:colorizer_fence")).addOptional(new ResourceLocation("immersiveengineering:steel_fence"));
//        this.tag(ModRegistry.NON_DIAGONAL_PANES_BLOCK_TAG);
    }
}
