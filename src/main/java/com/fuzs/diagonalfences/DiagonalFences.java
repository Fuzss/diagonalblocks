package com.fuzs.diagonalfences;

import com.fuzs.diagonalfences.element.DiagonalFencesElement;
import com.fuzs.puzzleslib_df.PuzzlesLib;
import com.fuzs.puzzleslib_df.element.AbstractElement;
import com.fuzs.puzzleslib_df.element.ElementRegistry;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.common.Mod;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@SuppressWarnings({"WeakerAccess", "unused"})
@Mod(DiagonalFences.MODID)
public class DiagonalFences extends PuzzlesLib {

    public static final String MODID = "diagonalfences";
    public static final String NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.NAME);

    public static final AbstractElement DIAGONAL_FENCES = register("diagonal_fences", DiagonalFencesElement::new, Dist.CLIENT);

    public DiagonalFences() {

        ElementRegistry.setup(MODID, false);
    }

}
