package fuzs.diagonalfences.client.model;

import com.google.common.base.Stopwatch;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.api.world.level.block.EightWayDirection;
import fuzs.diagonalfences.client.core.ClientAbstractions;
import fuzs.diagonalfences.mixin.client.accessor.*;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.MultiVariant;
import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.renderer.block.model.multipart.*;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.Direction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.apache.commons.lang3.ArrayUtils;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

public class MultipartAppender {

    public static void onPrepareModelBaking(ModelBakery modelBakery) {
        Stopwatch stopwatch = Stopwatch.createStarted();
        BuiltInRegistries.BLOCK.stream()
                .filter(block -> (block instanceof FenceBlock || block instanceof IronBarsBlock) && block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties())
                .map(block -> block.getStateDefinition().any())
                .forEach(state -> {

                    if (modelBakery.getModel(BlockModelShaper.stateToModelLocation(state)) instanceof MultiPart multiPart) {
                        appendDiagonalSelectors(((ModelBakeryAccessor) modelBakery)::diagonalfences$callCacheAndQueueDependencies, multiPart, state.getBlock() instanceof IronBarsBlock);
                    } else {
                        DiagonalFences.LOGGER.warn("Block '{}' is not using multipart models, diagonal connections will not be visible!", state.getBlock());
                    }
                });

        DiagonalFences.LOGGER.info("Constructing diagonal block models took {} milliseconds", stopwatch.stop().elapsed().toMillis());
    }

    /**
     * Append the multipart variant selectors needed for the diagonal arms of the fence, wall, etc.
     *
     * @param multiPart the original multipart unbaked variant
     */
    public static void appendDiagonalSelectors(BiConsumer<ResourceLocation, UnbakedModel> modelBakery, MultiPart multiPart, boolean rotateCenter) {

        List<Selector> selectors = multiPart.getSelectors();
        List<Selector> newSelectors = Lists.newArrayList();

        for (ListIterator<Selector> iterator = selectors.listIterator(); iterator.hasNext(); ) {
            Selector selector = iterator.next();

            Condition condition = ((SelectorAccessor) selector).diagonalfences$getCondition();
            ConditionFactoryPair conditionFactoryPair = findKeyValueCondition(condition, keyValueCondition -> EightWayDirection.byName(((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getKey()) != null);
            if (conditionFactoryPair != null) {

                EightWayDirection direction = EightWayDirection.byName(((KeyValueConditionAccessor) conditionFactoryPair.condition()).diagonalfences$getKey());
                if (direction != null) {

                    // intercardinal selectors are already present, either a resource pack has dedicated support for diagonal fences or the model baking event has run multiple times
                    // either way stop any processing of selectors, no new selectors have been added as that's done at the end of the method
                    if (direction.isIntercardinal()) return;

                    if (Objects.equals(((KeyValueConditionAccessor) conditionFactoryPair.condition()).diagonalfences$getValue(), "true")) {

                        // rotates vanilla cardinal direction model parts and adds them as new selectors, all that's necessary for fences
                        KeyValueCondition newCondition = new KeyValueCondition(direction.rotateClockWise().getSerializedName(), "true");
                        appendNewSelector(modelBakery, conditionFactoryPair.factory().apply(newCondition), selector, direction, newSelectors);
                    } else {

                        // this deals with the model part that shows on the side of the center post of glass panes when the model part corresponding to that direction is NOT being rendered
                        // we adjust the condition so it no longer renders when only two opposite intercardinal directions are present, since we handle that with our own model parts
                        Condition newCondition = negateCondition(new OrCondition(rotateCenterConditions().values()));
                        Selector newSelector = new Selector(new AndCondition(Lists.newArrayList(condition, newCondition)), selector.getVariant());
                        iterator.set(newSelector);

                        // the model parts used when only two opposite intercardinal directions are present
                        Condition otherNewCondition = getAndCondition(direction.rotateCounterClockWise(), direction.rotateCounterClockWise().opposite());
                        appendNewSelector(modelBakery, otherNewCondition, selector, direction.rotateClockWise().rotateClockWise(), newSelectors);
                    }
                }
            } else if (rotateCenter && condition == Condition.TRUE) {

                // this is the center post, we only use this for glass panes
                // it is rotated and used as a separate model part when only two opposite intercardinal directions are present
                // otherwise it renders normally, but we also change the vanilla condition to exclude the two cases that can result from our new model parts
                Map<EightWayDirection, Condition> conditions = rotateCenterConditions();
                for (Map.Entry<EightWayDirection, Condition> entry : conditions.entrySet()) {

                    appendNewSelector(modelBakery, entry.getValue(), selector, entry.getKey(), newSelectors);
                }

                Selector newSelector = new Selector(negateCondition(new OrCondition(conditions.values())), selector.getVariant());
                iterator.set(newSelector);
            }
        }

        selectors.addAll(newSelectors);
    }

    @Nullable
    private static MultipartAppender.ConditionFactoryPair findKeyValueCondition(Condition condition, Predicate<KeyValueCondition> filter) {
        if (condition instanceof KeyValueCondition keyValueCondition) {
            return filter.test(keyValueCondition) ? new ConditionFactoryPair(keyValueCondition, UnaryOperator.identity()) : null;
        } else if (condition instanceof AndCondition) {
            List<Condition> conditions = Lists.newArrayList(((AndConditionAccessor) condition).diagonalfences$getConditions());
            for (int i = 0; i < conditions.size(); i++) {
                ConditionFactoryPair conditionFactoryPair = findKeyValueCondition(conditions.get(i), filter);
                if (conditionFactoryPair != null) {
                    conditions.remove(conditionFactoryPair.condition());
                    return new ConditionFactoryPair(conditionFactoryPair.condition(), condition1 -> {
                        conditions.add(conditionFactoryPair.factory().apply(condition1));
                        return new AndCondition(conditions);
                    });
                }
            }
        } else if (condition instanceof OrCondition) {
            List<Condition> conditions = Lists.newArrayList(((OrConditionAccessor) condition).diagonalfences$getConditions());
            for (int i = 0; i < conditions.size(); i++) {
                ConditionFactoryPair conditionFactoryPair = findKeyValueCondition(conditions.get(i), filter);
                if (conditionFactoryPair != null) {
                    conditions.remove(conditionFactoryPair.condition());
                    return new ConditionFactoryPair(conditionFactoryPair.condition(), condition1 -> {
                        conditions.add(conditionFactoryPair.factory().apply(condition1));
                        return new OrCondition(conditions);
                    });
                }
            }
        }
        return null;
    }

    private static Map<EightWayDirection, Condition> rotateCenterConditions() {

        Map<EightWayDirection, Condition> conditions = Maps.newHashMap();
        for (EightWayDirection direction : EightWayDirection.getCardinalDirections()) {

            // we just need this once per axis
            if (direction.getX() == 1 || direction.getZ() == 1) {

                EightWayDirection interDirection = direction.rotateClockWise();
                Condition newCondition = getAndCondition(interDirection, interDirection.opposite());
                conditions.put(direction, newCondition);
            }
        }

        return conditions;
    }

    private static Condition negateCondition(Condition condition) {

        return stateDefinition -> condition.getPredicate(stateDefinition).negate();
    }

    private static Condition getAndCondition(EightWayDirection... directions) {

        List<Condition> conditions = Lists.newArrayList();
        for (EightWayDirection direction : EightWayDirection.values()) {

            String value = ArrayUtils.contains(directions, direction) ? "true" : "false";
            conditions.add(new KeyValueCondition(direction.getSerializedName(), value));
        }

        return new AndCondition(conditions);
    }

    private static void appendNewSelector(BiConsumer<ResourceLocation, UnbakedModel> modelBakery, Condition newCondition, Selector selector, EightWayDirection direction, List<Selector> newSelectors) {

        EightWayDirection interDirection = direction.rotateClockWise();
        List<Variant> variants = selector.getVariant().getVariants();
        List<Variant> newVariants = Lists.newArrayList();
        for (Variant variant : variants) {

            // Fabric Api's Model Api does not work as we need it to when using ModelResourceLocation, so stick to normal ResourceLocation
            ResourceLocation location = new ResourceLocation(variant.getModelLocation() + "_" + interDirection.getSerializedName());
            // this is the rotated model part, just make sure it is cached somewhere to avoid recreation multiple times since it's very expensive
            // we could also use our own cache, but unbaked models cache works fine
            modelBakery.accept(location, new RotatedVariant(variant, direction.toDirection()));
            // copy old variant which is now backed by the rotated model part, besides that only the weight value should really matter
            newVariants.add(new Variant(location, variant.getRotation(), variant.isUvLocked(), variant.getWeight()));
        }

        newSelectors.add(new Selector(newCondition, new MultiVariant(newVariants)));
    }

    /**
     * Duplicate and rotate all {@link BakedQuad quads} from the given {@link BakedModel segmentModel} to produce a new
     * variant for a diagonal segment
     */
    public static BakedModel rotateMultipartSegment(@Nullable BlockState state, BakedModel segmentModel, Direction armDir) {

        Map<Direction, List<BakedQuad>> quadMap = new HashMap<>();
        rotateQuads(quadMap, state, segmentModel, null, armDir);

        for (Direction cullFace : Direction.values()) {
            rotateQuads(quadMap, state, segmentModel, cullFace, armDir);
        }

        return ClientAbstractions.INSTANCE.createWrappedBakedModel(segmentModel, quadMap);
    }

    /**
     * Rotate all {@link BakedQuad}s with the given {@link Direction cullFace} of the given {@link BakedModel segmentModel} 45 degrees clockwise.
     * The quads are duplicated, rotated and have their vertex normals recalculated.
     */
    private static void rotateQuads(Map<Direction, List<BakedQuad>> quadMap, @Nullable BlockState state, BakedModel segmentModel, Direction cullFace, Direction segmentDir) {

        List<BakedQuad> quads = segmentModel.getQuads(state, cullFace, RandomSource.create());
        List<BakedQuad> newQuads = Lists.newArrayList();

        for (BakedQuad quad : quads) {
            BakedQuad copy = QuadUtils.duplicateQuad(quad);
            QuadUtils.rotateQuad(copy, segmentDir);
            newQuads.add(copy);
        }

        quadMap.put(cullFace, newQuads);
    }

    private record ConditionFactoryPair(KeyValueCondition condition, UnaryOperator<Condition> factory) {

    }
}
