package fuzs.diagonalfences;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.BlockTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.fml.common.Mod;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@SuppressWarnings("unused")
@Mod(DiagonalFences.MODID)
public class DiagonalFences {

    public static final String MODID = "diagonalfences";
    public static final String NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.NAME);

    public static final TagKey<Block> NON_DIAGONAL_FENCES_TAG = BlockTags.create(new ResourceLocation(DiagonalFences.MODID, "non_diagonal_fences"));

    public DiagonalFences() {

    }

}
