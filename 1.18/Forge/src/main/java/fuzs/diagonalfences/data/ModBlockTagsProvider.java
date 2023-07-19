package fuzs.diagonalfences.data;

import fuzs.diagonalfences.init.ModRegistry;
import fuzs.puzzleslib.api.data.v1.AbstractTagProvider;
import net.minecraft.data.DataGenerator;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.common.data.ExistingFileHelper;

public class ModBlockTagsProvider extends AbstractTagProvider.Blocks {

    public ModBlockTagsProvider(DataGenerator packOutput, String modId, ExistingFileHelper fileHelper) {
        super(packOutput, modId, fileHelper);
    }

    @Override
    protected void addTags() {
        this.tag(ModRegistry.NON_DIAGONAL_FENCES_BLOCK_TAG).addOptional(new ResourceLocation("assorteddecor:colorizer_fence")).addOptional(new ResourceLocation("immersiveengineering:steel_fence"));
        this.tag(ModRegistry.NON_DIAGONAL_PANES_BLOCK_TAG);
    }
}
