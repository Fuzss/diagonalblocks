package fuzs.diagonalfences.data.client;

import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.puzzlesaccessapi.api.client.data.v2.BlockModelBuilder;
import fuzs.puzzlesaccessapi.api.client.data.v2.ItemModelBuilder;
import fuzs.puzzleslib.api.client.data.v2.AbstractModelProvider;
import fuzs.puzzleslib.api.data.v2.core.DataProviderContext;
import net.minecraft.data.models.blockstates.MultiPartGenerator;
import net.minecraft.data.models.blockstates.Variant;
import net.minecraft.data.models.blockstates.VariantProperties;
import net.minecraft.data.models.model.ModelLocationUtils;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Blocks;

public class DynamicModelProvider extends AbstractModelProvider {

    public DynamicModelProvider(DataProviderContext context) {
        super(context);
    }

    @Override
    public void addBlockModels(BlockModelBuilder builder) {
        ResourceLocation resourceLocation = ModelLocationUtils.getModelLocation(Blocks.STONE);
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            type.getBlockConversions().values().forEach((block) -> {
                builder.getBlockStateOutput().accept(MultiPartGenerator.multiPart(block).with(Variant.variant().with(VariantProperties.MODEL, resourceLocation)));
                builder.skipAutoItemBlock(block);
            });
        }
    }

    @Override
    public void addItemModels(ItemModelBuilder builder) {

    }
}
