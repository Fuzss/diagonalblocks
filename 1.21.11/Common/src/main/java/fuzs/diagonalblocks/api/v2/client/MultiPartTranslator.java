package fuzs.diagonalblocks.api.v2.client;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockType;
import fuzs.diagonalblocks.impl.client.resources.model.MultiPartAppender;
import net.minecraft.client.renderer.block.model.BlockModelDefinition;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.world.level.block.state.properties.Property;

import java.util.List;
import java.util.Map;

public class MultiPartTranslator {
    private static final Map<DiagonalBlockType, MultiPartTranslator> TRANSLATORS = Maps.newConcurrentMap();

    protected final DiagonalBlockType diagonalBlockType;

    protected MultiPartTranslator(DiagonalBlockType diagonalBlockType) {
        this.diagonalBlockType = diagonalBlockType;
    }

    public static void register(DiagonalBlockType diagonalBlockType, MultiPartTranslator multiPartTranslator) {
        TRANSLATORS.put(diagonalBlockType, multiPartTranslator);
    }

    public static MultiPartTranslator get(DiagonalBlockType diagonalBlockType) {
        return TRANSLATORS.computeIfAbsent(diagonalBlockType, MultiPartTranslator::new);
    }

    protected Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
        return oldValue;
    }

    public BlockModelDefinition.MultiPartDefinition apply(BlockModelDefinition.MultiPartDefinition baseBlockModel) {
        return this.applyAdditionalSelectors(this.getModelFromBase(baseBlockModel));
    }

    protected BlockModelDefinition.MultiPartDefinition getModelFromBase(BlockModelDefinition.MultiPartDefinition multiPart) {
        List<Selector> selectors = Lists.newArrayList(multiPart.selectors());
        return new BlockModelDefinition.MultiPartDefinition(selectors);
    }

    protected BlockModelDefinition.MultiPartDefinition applyAdditionalSelectors(BlockModelDefinition.MultiPartDefinition multiPart) {
        return MultiPartAppender.appendDiagonalSelectors(multiPart, false);
    }

    public boolean allowBaseModelAsFallback() {
        return true;
    }

    @Override
    public String toString() {
        return "MultiPartTranslator[" + this.diagonalBlockType + "]";
    }
}
