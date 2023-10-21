package fuzs.diagonalfences;

import fuzs.diagonalfences.data.DynamicBlockLootProvider;
import fuzs.diagonalfences.data.DynamicBlockTagsProvider;
import fuzs.diagonalfences.handler.WallBlockHandler;
import fuzs.diagonalfences.init.ModRegistry;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.api.core.v1.context.PackRepositorySourcesContext;
import fuzs.puzzleslib.api.event.v1.RegistryEntryAddedCallback;
import fuzs.puzzleslib.api.resources.v1.DynamicPackResources;
import fuzs.puzzleslib.api.resources.v1.PackResourcesHelper;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DiagonalFences implements ModConstructor {
    public static final String MOD_ID = "diagonalfences";
    public static final String MOD_NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.MOD_NAME);

    @Override
    public void onConstructMod() {
        ModRegistry.touch();
        registerHandlers();
    }

    private static void registerHandlers() {
        RegistryEntryAddedCallback.registryEntryAdded(Registries.BLOCK).register(WallBlockHandler::onBlockAdded);
        RegistryEntryAddedCallback.registryEntryAdded(Registries.ITEM).register(WallBlockHandler::onItemAdded);
    }

    @Override
    public void onAddDataPackFinders(PackRepositorySourcesContext context) {
        context.addRepositorySource(PackResourcesHelper.buildServerPack(id("dynamic_walls"), DynamicPackResources.create(DynamicBlockLootProvider::new, DynamicBlockTagsProvider::new), false));
    }

    public static ResourceLocation id(String path) {
        return new ResourceLocation(MOD_ID, path);
    }
}
