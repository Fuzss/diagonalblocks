package com.fuzs.diagonalfences.client.element;

import com.fuzs.diagonalfences.block.IEightWayBlock;
import com.fuzs.diagonalfences.client.renderer.model.BlockAssetGenerator;
import com.fuzs.diagonalfences.element.DiagonalWindowsElement;
import com.fuzs.diagonalfences.resources.IResourceInfoFactory;
import com.fuzs.diagonalfences.resources.RuntimeResourcePack;
import com.fuzs.puzzleslib_df.element.extension.ElementExtension;
import com.fuzs.puzzleslib_df.element.side.IClientElement;
import com.google.common.collect.Lists;
import net.minecraft.block.Block;
import net.minecraft.block.PaneBlock;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.model.BlockPart;
import net.minecraft.client.renderer.model.BlockPartRotation;
import net.minecraft.resources.ResourcePackInfo;
import net.minecraft.resources.ResourcePackList;
import net.minecraft.state.BooleanProperty;
import net.minecraft.util.Direction;
import net.minecraft.util.math.vector.Vector3f;
import net.minecraftforge.client.event.ParticleFactoryRegisterEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class DiagonalWindowsExtension extends ElementExtension<DiagonalWindowsElement> implements IClientElement {

    private BlockAssetGenerator generator;

    public DiagonalWindowsExtension(DiagonalWindowsElement parent) {

        super(parent);
    }

    @Override
    public void setupClient() {

        // we just need an event which is called before ResourceManager is loaded for the first time
        // (which is during construction of Minecraft.class), but after registries have been populated
        // also our resource pack needs to be added earlier, so can't just do everything at once
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::onParticleFactoryRegister);

        Minecraft mc = Minecraft.getInstance();
        ResourcePackList packList = mc.getResourcePackList();
        this.generator = new BlockAssetGenerator(mc.getResourceManager());
        RuntimeResourcePack resourcePack = new RuntimeResourcePack(this.generator, "Diagonal Windows", this.parent.getDescription()[0]);
        this.addPackFinder(packList, resourcePack);
    }

    private void onParticleFactoryRegister(final ParticleFactoryRegisterEvent evt) {

        this.addUnits();
    }

    private void addPackFinder(ResourcePackList packList, IResourceInfoFactory resourcePack) {

        packList.addPackFinder((infoConsumer, infoFactory) -> {

            ResourcePackInfo resourcepackinfo = resourcePack.createResourcePack("diagonalwindows", true, ResourcePackInfo.Priority.TOP, true, true);
            infoConsumer.accept(resourcepackinfo);
        });
    }

    private void addUnits() {

        Set<Block> allFences = ForgeRegistries.BLOCKS.getValues().stream()
                .filter(block -> block instanceof PaneBlock && ((IEightWayBlock) block).hasProperties())
                .collect(Collectors.toSet());
        List<BooleanProperty> properties = new ArrayList<>(IEightWayBlock.DIRECTION_TO_PROPERTY_MAP.values());
        Map<Pair<String, String>, String> propertyConverter = IntStream.range(0, properties.size() / 2)
                .boxed()
                .map(i -> Pair.of(properties.get(i), properties.get(i + 4).getName()))
                .collect(Collectors.toMap(entry -> Pair.of(entry.getKey().getName(), entry.getKey().getName(true)), Pair::getValue));

        this.generator.addUnits(allFences, propertyConverter, this::modifyElements);
    }

    private void modifyElements(List<BlockPart> elements) {

        List<BlockPart> rotatedElements = Lists.newArrayList();
        for (BlockPart blockPart : elements) {

            // elements with a rotation are ignored as they tend to look worse when rotated wrongly over missing completely
            // might be best to manually add blocks with such elements to a blacklist
            if (blockPart.partRotation == null || blockPart.partRotation.angle == 0.0F) {

                final float center = 8.0F;
                Vector3f positionFrom = this.rescalePosition(blockPart.positionFrom, center);
                Vector3f positionTo = this.rescalePosition(blockPart.positionTo, center);
                BlockPartRotation rotation = new BlockPartRotation(new Vector3f(center, center, center), Direction.Axis.Y, -45.0F, false);
                blockPart = new BlockPart(positionFrom, positionTo, blockPart.mapFaces, rotation, blockPart.shade);
                rotatedElements.add(blockPart);
            }
        }

        elements.clear();
        elements.addAll(rotatedElements);
    }

    @SuppressWarnings("SameParameterValue")
    private Vector3f rescalePosition(Vector3f position, float center) {

        // cos(-pi/4)
        final float angle = 0.7071067812F;
        // decrease height slightly to prevent z-fighting
        float posY = position.getY();
        if (posY == 0.0F) {

            posY = 0.01F;
        } else if (posY == 16.0F) {

            posY = 15.99F;
        }

        // make texture start in the middle, otherwise a gap would be left
        float posZ = position.getZ();
        if (Math.abs(posZ - center) <= 1.0F) {

            posZ = center;
        }

        return new Vector3f(position.getX(), posY, (posZ - center) / angle + center);
    }

}
