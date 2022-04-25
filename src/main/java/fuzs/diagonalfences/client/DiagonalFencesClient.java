package fuzs.diagonalfences.client;

import com.google.common.collect.Lists;
import com.mojang.math.Vector3f;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.block.IEightWayBlock;
import fuzs.diagonalfences.client.renderer.model.BlockAssetGenerator;
import fuzs.diagonalfences.resources.RuntimeResourcePack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.block.model.BlockElement;
import net.minecraft.client.renderer.block.model.BlockElementRotation;
import net.minecraft.core.Direction;
import net.minecraft.server.packs.PackType;
import net.minecraft.server.packs.repository.*;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ParticleFactoryRegisterEvent;
import net.minecraftforge.event.AddPackFindersEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.registries.ForgeRegistries;
import org.apache.commons.lang3.tuple.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@Mod.EventBusSubscriber(modid = DiagonalFences.MODID, bus = Mod.EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
public class DiagonalFencesClient {

    private static BlockAssetGenerator generator;

    @SubscribeEvent
    public static void onAddPackFinders(AddPackFindersEvent event) {

        if (event.getPackType() != PackType.CLIENT_RESOURCES) {

            return;
        }

        Minecraft mc = Minecraft.getInstance();
        generator = new BlockAssetGenerator(mc.getResourceManager());
        RuntimeResourcePack resourcePack = new RuntimeResourcePack(generator, DiagonalFences.NAME, "Fences connecting diagonally? Wait. That's illegal.");

        event.addRepositorySource((packConsumer, packConstructor) -> {

            Pack resourcepackinfo = resourcePack.createResourcePack(DiagonalFences.MODID, true, Pack.Position.TOP, true, true);
            packConsumer.accept(resourcepackinfo);
        });
    }

    @SubscribeEvent
    public static void onParticleFactoryRegister(final ParticleFactoryRegisterEvent evt) {

        //TODO: try to find a better place for this without abusing an unrelated event
        addUnits();
    }

    private static void addUnits() {

        Set<Block> allFences = ForgeRegistries.BLOCKS.getValues().stream()
                .filter(block -> block instanceof FenceBlock && ((IEightWayBlock) block).hasProperties())
                .collect(Collectors.toSet());
        List<BooleanProperty> properties = new ArrayList<>(IEightWayBlock.DIRECTION_TO_PROPERTY_MAP.values());
        Map<Pair<String, String>, String> propertyConverter = IntStream.range(0, properties.size() / 2)
                .boxed()
                .map(i -> Pair.of(properties.get(i), properties.get(i + 4).getName()))
                .collect(Collectors.toMap(entry -> Pair.of(entry.getKey().getName(), entry.getKey().getName(true)), Pair::getValue));

        generator.addUnits(allFences, propertyConverter, DiagonalFencesClient::modifyElements);
    }

    private static void modifyElements(List<BlockElement> elements) {

        List<BlockElement> rotatedElements = Lists.newArrayList();
        for (BlockElement blockElement : elements) {

            // elements with a rotation are ignored as they tend to look worse when rotated wrongly over missing completely
            // might be best to manually add blocks with such elements to a blacklist
            if (blockElement.rotation == null || blockElement.rotation.angle == 0.0F) {

                final float center = 8.0F;
                Vector3f positionFrom = rescalePosition(blockElement.from, center);
                Vector3f positionTo = rescalePosition(blockElement.to, center);
                BlockElementRotation rotation = new BlockElementRotation(new Vector3f(center, center, center), Direction.Axis.Y, -45.0F, false);
                blockElement = new BlockElement(positionFrom, positionTo, blockElement.faces, rotation, blockElement.shade);
                rotatedElements.add(blockElement);
            }
        }

        elements.clear();
        elements.addAll(rotatedElements);
    }

    @SuppressWarnings("SameParameterValue")
    private static Vector3f rescalePosition(Vector3f position, float center) {

        // cos(-pi/4)
        final float angle = 0.7071067812F;
        return new Vector3f(position.x(), position.y(), (position.z() - center) / angle + center);
    }

}
