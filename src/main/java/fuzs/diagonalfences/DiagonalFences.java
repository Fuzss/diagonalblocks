package fuzs.diagonalfences;

import fuzs.diagonalfences.element.DiagonalFencesElement;
import fuzs.puzzleslib.PuzzlesLib;
import fuzs.puzzleslib.element.AbstractElement;
import net.minecraftforge.fml.common.Mod;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@SuppressWarnings("unused")
@Mod(DiagonalFences.MODID)
public class DiagonalFences {

    public static final String MODID = "diagonalfences";
    public static final String NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.NAME);

    public static final AbstractElement DIAGONAL_FENCES = PuzzlesLib.register("diagonal_fences", DiagonalFencesElement::new);

    public DiagonalFences() {

        PuzzlesLib.setup(MODID, false);
    }

}
