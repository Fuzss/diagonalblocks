package fuzs.diagonalfences;

import fuzs.diagonalfences.init.ModRegistry;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import fuzs.puzzleslib.api.event.v1.RegistryEntryAddedCallback;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class DiagonalFences implements ModConstructor {
    public static final String MOD_ID = "diagonalfences";
    public static final String MOD_NAME = "Diagonal Fences";
    public static final Logger LOGGER = LogManager.getLogger(DiagonalFences.MOD_NAME);

    @Override
    public void onConstructMod() {
        ModRegistry.touch();
        RegistryEntryAddedCallback.registryEntryAdded(Registries.BLOCK).register((ResourceLocation id, Block entry, BiConsumer<ResourceLocation, Supplier<Block>> registrar) -> {
            if (entry instanceof FenceBlock) {
                id = id("additional_" + id.getPath());
                registrar.accept(id, () -> new Block(BlockBehaviour.Properties.of()));
                LOGGER.error("Added {}", id);
            }
        });
    }

    public static ResourceLocation id(String path) {
        return new ResourceLocation(MOD_ID, path);
    }
}
