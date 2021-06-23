package fuzs.diagonalfences.resources;

import net.minecraft.resources.ResourcePackInfo;
import net.minecraft.util.text.StringTextComponent;

public interface IResourceInfoFactory {

    String getName();

    StringTextComponent getDescription();

    ResourcePackInfo createResourcePack(String owner, boolean alwaysEnabled, ResourcePackInfo.Priority priority, boolean orderLocked, boolean hidden);

}
