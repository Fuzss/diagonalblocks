package com.fuzs.puzzleslib_df.element.extension;

import com.fuzs.puzzleslib_df.element.AbstractElement;
import com.fuzs.puzzleslib_df.element.side.ICommonElement;
import com.fuzs.puzzleslib_df.element.side.ISidedElement;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.loading.FMLEnvironment;

import java.util.function.Function;

/**
 * an element that can be extended to one side which will only be called when it matches the current physical side
 * @param <T> extension class
 */
public abstract class ExtensibleElement<T extends ElementExtension<?> & ISidedElement> extends AbstractElement implements ICommonElement {

    /**
     * extension class object
     */
    public final T extension;

    /**
     * @param extension provider for extension
     * @param dist physical side required for extension to be created
     */
    public ExtensibleElement(Function<ExtensibleElement<?>, T> extension, Dist dist) {

        this.extension = dist == FMLEnvironment.dist ? extension.apply(this) : null;
    }

}
