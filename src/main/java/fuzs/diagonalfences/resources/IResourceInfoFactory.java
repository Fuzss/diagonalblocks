package fuzs.diagonalfences.resources;

import net.minecraft.server.packs.repository.Pack;
import net.minecraft.network.chat.TextComponent;

public interface IResourceInfoFactory {

    String getName();

    TextComponent getDescription();

    Pack createResourcePack(String owner, boolean alwaysEnabled, Pack.Position position, boolean orderLocked, boolean hidden);

}
