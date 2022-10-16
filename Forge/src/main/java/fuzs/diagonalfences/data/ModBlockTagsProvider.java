package fuzs.diagonalfences.data;

import fuzs.diagonalfences.init.ModRegistry;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.tags.BlockTagsProvider;
import net.minecraftforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.Nullable;

public class ModBlockTagsProvider extends BlockTagsProvider {

    public ModBlockTagsProvider(DataGenerator p_126511_, String modId, @Nullable ExistingFileHelper existingFileHelper) {
        super(p_126511_, modId, existingFileHelper);
    }

    @Override
    protected void addTags() {
        this.tag(ModRegistry.NON_DIAGONAL_FENCES_TAG);
        this.tag(ModRegistry.NON_DIAGONAL_FENCE_GATES_TAG);
    }
}
