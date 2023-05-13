package fuzs.diagonalfences.client.model;

import com.google.common.base.Stopwatch;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.api.world.level.block.EightWayDirection;
import fuzs.diagonalfences.client.core.ClientAbstractions;
import fuzs.diagonalfences.mixin.client.accessor.KeyValueConditionAccessor;
import fuzs.diagonalfences.mixin.client.accessor.ModelBakeryAccessor;
import fuzs.diagonalfences.mixin.client.accessor.SelectorAccessor;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.MultiVariant;
import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.renderer.block.model.multipart.*;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraft.client.resources.model.UnbakedModel;
import net.minecraft.core.Direction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockState;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.logging.log4j.util.BiConsumer;
import org.jetbrains.annotations.Nullable;

import java.util.*;

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

        DiagonalFences.LOGGER.info("Constructing diagonal block models took {}ms", stopwatch.stop().elapsed().toMillis());
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
            if (condition instanceof KeyValueCondition keyValueCondition) {

                EightWayDirection direction = EightWayDirection.byName(((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getKey());
                if (direction != null) {

                    if (Objects.equals(((KeyValueConditionAccessor) keyValueCondition).diagonalfences$getValue(), "true")) {

                        KeyValueCondition newCondition = new KeyValueCondition(direction.rotateClockWise().getSerializedName(), "true");
                        appendNewConditions(modelBakery, newCondition, newSelectors, selector, direction);
                    } else {

                        Condition newCondition = negateCondition(new OrCondition(rotateCenterConditions().values()));
                        Selector newSelector = new Selector(new AndCondition(Lists.newArrayList(condition, newCondition)), selector.getVariant());
                        iterator.set(newSelector);
                    }
                }
            } else if (rotateCenter && condition == Condition.TRUE) {

                Map<EightWayDirection, Condition> conditions = rotateCenterConditions();
                for (Map.Entry<EightWayDirection, Condition> entry : conditions.entrySet()) {

                    appendNewConditions(modelBakery, entry.getValue(), newSelectors, selector, entry.getKey());
                }

                Selector newSelector = new Selector(negateCondition(new OrCondition(conditions.values())), selector.getVariant());
                iterator.set(newSelector);
            }
        }

        selectors.addAll(newSelectors);
    }

    private static Map<EightWayDirection, Condition> rotateCenterConditions() {

        Map<EightWayDirection, Condition> conditions = Maps.newHashMap();
        for (EightWayDirection direction : EightWayDirection.getCardinalDirections()) {

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

    private static void appendNewConditions(BiConsumer<ResourceLocation, UnbakedModel> modelBakery, Condition newCondition, List<Selector> newSelectors, Selector selector, EightWayDirection direction) {

        EightWayDirection interDirection = direction.rotateClockWise();
        List<Variant> variants = selector.getVariant().getVariants();
        List<Variant> newVariants = Lists.newArrayList();
        for (Variant variant : variants) {

            ModelResourceLocation location = new ModelResourceLocation(variant.getModelLocation(), interDirection.getSerializedName());
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
}
