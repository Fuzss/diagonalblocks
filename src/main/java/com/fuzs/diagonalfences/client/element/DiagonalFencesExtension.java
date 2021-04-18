package com.fuzs.diagonalfences.client.element;

import com.fuzs.diagonalfences.DiagonalFences;
import com.fuzs.diagonalfences.block.IEightWayBlock;
import com.fuzs.diagonalfences.client.renderer.model.BlockAssetGenerator;
import com.fuzs.diagonalfences.element.DiagonalFencesElement;
import com.fuzs.diagonalfences.resources.IResourceInfoFactory;
import com.fuzs.diagonalfences.resources.RuntimeResourcePack;
import com.fuzs.puzzleslib_df.element.extension.ElementExtension;
import com.fuzs.puzzleslib_df.element.side.IClientElement;
import com.google.common.collect.Lists;
import net.minecraft.block.Block;
import net.minecraft.block.FenceBlock;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.model.BlockPart;
import net.minecraft.client.renderer.model.BlockPartRotation;
import net.minecraft.resources.ResourcePackInfo;
import net.minecraft.resources.ResourcePackList;
import net.minecraft.state.Property;
import net.minecraft.util.Direction;
import net.minecraft.util.math.vector.Vector3f;
import net.minecraftforge.client.event.ParticleFactoryRegisterEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class DiagonalFencesExtension extends ElementExtension<DiagonalFencesElement> implements IClientElement {

    private BlockAssetGenerator generator;

    public DiagonalFencesExtension(DiagonalFencesElement parent) {

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
        RuntimeResourcePack resourcePack = new RuntimeResourcePack(this.generator, DiagonalFences.NAME, this.parent.getDescription()[0]);
        this.addPackFinder(packList, resourcePack);
    }

    private void onParticleFactoryRegister(final ParticleFactoryRegisterEvent evt) {

        this.addUnits();
    }

    private void addPackFinder(ResourcePackList packList, IResourceInfoFactory resourcePack) {

        packList.addPackFinder((infoConsumer, infoFactory) -> {

            ResourcePackInfo resourcepackinfo = resourcePack.createResourcePack(DiagonalFences.MODID, true, ResourcePackInfo.Priority.TOP, true, true);
            infoConsumer.accept(resourcepackinfo);
        });
    }

    private void addUnits() {

        Set<Block> allFences = ForgeRegistries.BLOCKS.getValues().stream()
                .filter(block -> block instanceof FenceBlock && ((IEightWayBlock) block).hasProperties())
                .collect(Collectors.toSet());
        List<Property<?>> properties = new ArrayList<>(IEightWayBlock.DIRECTION_TO_PROPERTY_MAP.values());
        Map<Property<?>, Property<?>> propertyConverter = IntStream.range(0, properties.size() / 2)
                .boxed().collect(Collectors.toMap(properties::get, i -> properties.get(i + 4)));

        this.generator.addUnits(allFences, propertyConverter, this::modifyElements);
    }

    private void modifyElements(List<BlockPart> elements) {

        List<BlockPart> rotatedElements = Lists.newArrayList();
        for (BlockPart blockPart : elements) {

            // elements with a rotation are ignored as they tend to look worse when rotated wrongly over missing completely
            // might be best to manually add blocks with such elements to a blacklist
            if (blockPart.partRotation == null || blockPart.partRotation.angle == 0.0F) {

                // cos(-pi/4)
                final float angle = 0.7071067812F;
                final float center = 8.0F;
                Vector3f positionFrom = new Vector3f(blockPart.positionFrom.getX(), blockPart.positionFrom.getY(), (blockPart.positionFrom.getZ() - center) / angle + center);
                Vector3f positionTo = new Vector3f(blockPart.positionTo.getX(), blockPart.positionTo.getY(), (blockPart.positionTo.getZ() - center) / angle + center);
                BlockPartRotation rotation = new BlockPartRotation(new Vector3f(center, center, center), Direction.Axis.Y, -45.0F, false);
                blockPart = new BlockPart(positionFrom, positionTo, blockPart.mapFaces, rotation, blockPart.shade);
                rotatedElements.add(blockPart);
            }
        }

        elements.clear();
        elements.addAll(rotatedElements);
    }

}
