package fuzs.diagonalfences.data;

import fuzs.diagonalfences.init.ModRegistry;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.tags.BlockTagsProvider;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.Nullable;

public class ModBlockTagsProvider extends BlockTagsProvider {

    public ModBlockTagsProvider(DataGenerator pGenerator, String modId, @Nullable ExistingFileHelper existingFileHelper) {
        super(pGenerator, modId, existingFileHelper);
    }

    @Override
    protected void addTags() {
        this.tag(ModRegistry.NON_DIAGONAL_FENCES_BLOCK_TAG).addOptional(new ResourceLocation("assorteddecor:colorizer_fence")).addOptional(new ResourceLocation("immersiveengineering:steel_fence"));
        this.tag(ModRegistry.NON_DIAGONAL_PANES_BLOCK_TAG);
    }
}
