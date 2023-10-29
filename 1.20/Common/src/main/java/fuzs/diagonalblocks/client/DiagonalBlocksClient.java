package fuzs.diagonalblocks.client;

import com.google.common.collect.Lists;
import fuzs.diagonalblocks.DiagonalBlocks;
import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.client.MultiPartTranslator;
import fuzs.diagonalblocks.client.core.ClientAbstractions;
import fuzs.diagonalblocks.client.handler.DiagonalModelHandler;
import fuzs.diagonalblocks.client.resources.model.MultipartAppender;
import fuzs.diagonalblocks.data.client.DynamicModelProvider;
import fuzs.diagonalblocks.mixin.client.accessor.KeyValueConditionAccessor;
import fuzs.diagonalblocks.mixin.client.accessor.SelectorAccessor;
import fuzs.puzzleslib.api.client.core.v1.ClientModConstructor;
import fuzs.puzzleslib.api.client.event.v1.ModelEvents;
import fuzs.puzzleslib.api.core.v1.context.PackRepositorySourcesContext;
import fuzs.puzzleslib.api.event.v1.LoadCompleteCallback;
import fuzs.puzzleslib.api.resources.v1.DynamicPackResources;
import fuzs.puzzleslib.api.resources.v1.PackResourcesHelper;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.level.block.state.properties.WallSide;

import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.function.BiConsumer;

public class DiagonalBlocksClient implements ClientModConstructor {

    @Override
    public void onConstructMod() {
        registerHandlers();
    }

    private static void registerHandlers() {
        ModelEvents.MODIFY_UNBAKED_MODEL.register(DiagonalModelHandler::onModifyUnbakedModel);
        ModelEvents.AFTER_MODEL_LOADING.register(DiagonalModelHandler::onAfterModelLoading);
        LoadCompleteCallback.EVENT.register(() -> {
            // run a custom implementation here, the appropriate method in client mod constructor runs together with other mods, so we might miss some entries
            for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
                for (Map.Entry<Block, Block> entry : type.getBlockConversions().entrySet()) {
                    RenderType renderType = fuzs.puzzleslib.api.client.core.v1.ClientAbstractions.INSTANCE.getRenderType(entry.getKey());
                    ClientAbstractions.INSTANCE.registerRenderType(entry.getValue(), renderType);
                }
            }
        });
    }

    @Override
    public void onClientSetup() {
        MultiPartTranslator.register(DiagonalBlockTypes.WINDOW, new MultiPartTranslator(DiagonalBlockTypes.WINDOW) {

            @Override
            protected MultiPart applyAdditionalSelectors(BiConsumer<ResourceLocation, UnbakedModel> modelAdder, MultiPart multiPart) {
                return MultipartAppender.appendDiagonalSelectors(modelAdder, multiPart, true);
            }
        });
        MultiPartTranslator.register(DiagonalBlockTypes.WALL, new MultiPartTranslator(DiagonalBlockTypes.WALL) {

            @Override
            protected Comparable<?> getNewPropertyValue(Property<?> oldProperty, Property<?> newProperty, Comparable<?> oldValue) {
                if (newProperty.getValueClass() == WallSide.class) {
                    return (Boolean) oldValue ? WallSide.LOW : WallSide.NONE;
                } else {
                    return super.getNewPropertyValue(oldProperty, newProperty, oldValue);
                }
            }

            @Override
            protected MultiPart getModelFromBase(Block diagonalBlock, MultiPart baseBlockModel) {
                List<Selector> selectors = Lists.newArrayList(baseBlockModel.getSelectors());
                ListIterator<Selector> iterator = selectors.listIterator();
                while (iterator.hasNext()) {
                    Selector selector = iterator.next();
                    if (((SelectorAccessor) selector).diagonalfences$getCondition() instanceof KeyValueCondition keyValueCondition) {
                        String value = ((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getValue();
                        if (value.equals(WallSide.LOW.toString())) {
                            value = Boolean.TRUE.toString();
                        } else if (value.equals(WallSide.NONE.toString())) {
                            value = Boolean.FALSE.toString();
                        } else if (value.equals(WallSide.TALL.toString())) {
                            value = null;
                        } else {
                            continue;
                        }
                        if (value != null) {
                            String key = ((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getKey();
                            iterator.set(new Selector(new KeyValueCondition(key, value), selector.getVariant()));
                        } else {
                            iterator.remove();
                        }
                    }
                }
                return new MultiPart(diagonalBlock.getStateDefinition(), selectors);
            }
        });
    }

    @Override
    public void onAddResourcePackFinders(PackRepositorySourcesContext context) {
        context.addRepositorySource(PackResourcesHelper.buildClientPack(DiagonalBlocks.id("default_block_models"), DynamicPackResources.create(DynamicModelProvider::new), true));
    }
}
