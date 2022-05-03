package fuzs.diagonalfences;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.BlockTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.fml.common.Mod;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(DiagonalFences.MOD_ID)
public class DiagonalFences {
    public static final String MOD_ID = "diagonalfences";
    public static final String MOD_NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.MOD_NAME);

    public static final TagKey<Block> NON_DIAGONAL_FENCES_TAG = BlockTags.create(new ResourceLocation(DiagonalFences.MOD_ID, "non_diagonal_fences"));
}
