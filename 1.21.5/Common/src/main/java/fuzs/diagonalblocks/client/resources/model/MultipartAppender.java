package fuzs.diagonalblocks.client.resources.model;

import com.google.common.collect.Lists;
import fuzs.diagonalblocks.api.v2.EightWayDirection;
import fuzs.diagonalblocks.api.v2.impl.StarCollisionBlock;
import fuzs.puzzleslib.api.client.renderer.v1.model.QuadUtils;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.Util;
import net.minecraft.client.data.models.blockstates.ConditionBuilder;
import net.minecraft.client.renderer.block.model.*;
import net.minecraft.client.renderer.block.model.multipart.CombinedCondition;
import net.minecraft.client.renderer.block.model.multipart.Condition;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.ModelBaker;
import net.minecraft.client.resources.model.QuadCollection;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.StateHolder;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import org.apache.commons.lang3.ArrayUtils;
import org.jetbrains.annotations.Nullable;
import org.joml.Matrix4f;
import org.joml.Quaternionf;
import org.joml.Vector3f;
import org.joml.Vector4f;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

public class MultipartAppender {
    private static final float ROTATION_ANGLE = -45F * 0.017453292F;
    /**
     * Scale factor at a 45 degree rotation
     */
    private static final float SCALE_ROTATION_45 = 1.0F / (float) Math.cos(Math.PI / 4.0) - 1.0F;
    private static final Vector3f ROTATION_ORIGIN = new Vector3f(0.5F, 0.5F, 0.5F);
    private static final Matrix4f ROTATION_MATRIX = new Matrix4f().rotation(new Quaternionf().setAngleAxis(
            ROTATION_ANGLE,
            0.0F,
            1.0F,
            0.0F));

    /**
     * Append the multipart variant selectors needed for the diagonal arms of the fence, wall, etc.
     *
     * @param multiPart the original multipart unbaked variant
     */
    public static BlockModelDefinition.MultiPartDefinition appendDiagonalSelectors(BlockModelDefinition.MultiPartDefinition multiPart, boolean rotateCenter) {

        List<Selector> selectors = new ArrayList<>(multiPart.selectors());
        List<Selector> newSelectors = new ArrayList<>();

        for (ListIterator<Selector> iterator = selectors.listIterator(); iterator.hasNext(); ) {
            Selector selector = iterator.next();

            Condition condition = selector.condition().orElse(null);
            ConditionWithFactory conditionWithFactory = findKeyValueCondition(condition,
                    keyValueCondition -> EightWayDirection.byName(keyValueCondition.key()) != null);
            if (conditionWithFactory != null) {

                EightWayDirection direction = EightWayDirection.byName(conditionWithFactory.key());
                if (direction != null) {

                    // intercardinal selectors are already present, either a resource pack has dedicated support for diagonal fences or the model baking event has run multiple times
                    // either way stop any processing of selectors, no new selectors have been added as that's done at the end of the method
                    if (direction.isIntercardinal()) return multiPart;

                    if (Objects.equals(conditionWithFactory.value(), "true")) {

                        // rotates vanilla cardinal direction model parts and adds them as new selectors, all that's necessary for fences
                        Condition newCondition = new ConditionBuilder().term(StarCollisionBlock.PROPERTY_BY_DIRECTION.get(
                                direction.rotateClockWise()), Boolean.TRUE).build();
                        appendNewSelector(conditionWithFactory.factory().apply(newCondition),
                                selector,
                                direction,
                                newSelectors);
                    } else {

                        // this deals with the model part that shows on the side of the center post of glass panes when the model part corresponding to that direction is NOT being rendered
                        // we adjust the condition so it no longer renders when only two opposite intercardinal directions are present, since we handle that with our own model parts
                        Condition newCondition = negateCondition(new CombinedCondition(CombinedCondition.Operation.OR,
                                List.copyOf(rotateCenterConditions().values())));
                        Selector newSelector = new Selector(Optional.of(new CombinedCondition(CombinedCondition.Operation.AND,
                                Lists.newArrayList(condition, newCondition))), selector.variant());
                        iterator.set(newSelector);

                        // the model parts used when only two opposite intercardinal directions are present
                        Condition otherNewCondition = getAndCondition(direction.rotateCounterClockWise(),
                                direction.rotateCounterClockWise().getOpposite());
                        appendNewSelector(otherNewCondition,
                                selector,
                                direction.rotateClockWise().rotateClockWise(),
                                newSelectors);
                    }
                }
            } else if (rotateCenter && condition == null) {

                // this is the center post, we only use this for glass panes
                // it is rotated and used as a separate model part when only two opposite intercardinal directions are present
                // otherwise it renders normally, but we also change the vanilla condition to exclude the two cases that can result from our new model parts
                Map<EightWayDirection, Condition> conditions = rotateCenterConditions();
                for (Map.Entry<EightWayDirection, Condition> entry : conditions.entrySet()) {

                    appendNewSelector(entry.getValue(), selector, entry.getKey(), newSelectors);
                }

                Selector newSelector = new Selector(Optional.of(negateCondition(new CombinedCondition(CombinedCondition.Operation.OR,
                        List.copyOf(conditions.values())))), selector.variant());
                iterator.set(newSelector);
            }
        }

        selectors.addAll(newSelectors);
        return new BlockModelDefinition.MultiPartDefinition(selectors);
    }

    @Nullable
    private static MultipartAppender.ConditionWithFactory findKeyValueCondition(@Nullable Condition condition, Predicate<ConditionWithFactory> filter) {
        if (condition instanceof KeyValueCondition keyValueCondition) {
            if (keyValueCondition.tests().isEmpty()) {
                return null;
            } else if (keyValueCondition.tests().size() == 1 &&
                    keyValueCondition.tests().entrySet().iterator().next().getValue().entries().size() == 1) {
                Map.Entry<String, KeyValueCondition.Terms> entry = keyValueCondition.tests()
                        .entrySet()
                        .iterator()
                        .next();
                ConditionWithFactory conditionWithFactory = new ConditionWithFactory(entry.getKey(),
                        entry.getValue().entries().getFirst().value(),
                        UnaryOperator.identity());
                if (filter.test(conditionWithFactory)) {
                    return conditionWithFactory;
                } else {
                    return null;
                }
            } else {
                List<Condition> conditions = new ArrayList<>();
                for (Map.Entry<String, KeyValueCondition.Terms> entry : keyValueCondition.tests().entrySet()) {
                    for (KeyValueCondition.Term term : entry.getValue().entries()) {
                        conditions.add(new KeyValueCondition(Collections.singletonMap(entry.getKey(),
                                new KeyValueCondition.Terms(Collections.singletonList(term)))));
                    }
                }
                return findKeyValueCondition(new CombinedCondition(CombinedCondition.Operation.AND, conditions),
                        filter);
            }
        } else if (
                condition instanceof CombinedCondition(CombinedCondition.Operation operation, List<Condition> terms) &&
                        operation == CombinedCondition.Operation.AND) {
            List<Condition> conditions = new ArrayList<>(terms);
            for (int i = 0; i < conditions.size(); i++) {
                ConditionWithFactory conditionWithFactory = findKeyValueCondition(conditions.get(i), filter);
                if (conditionWithFactory != null) {
                    conditions.remove(i);
                    return new ConditionWithFactory(conditionWithFactory.key(),
                            conditionWithFactory.value(),
                            condition1 -> {
                                conditions.add(conditionWithFactory.factory().apply(condition1));
                                return new CombinedCondition(CombinedCondition.Operation.AND, conditions);
                            });
                }
            }
        } else if (
                condition instanceof CombinedCondition(CombinedCondition.Operation operation, List<Condition> terms) &&
                        operation == CombinedCondition.Operation.OR) {
            List<Condition> conditions = new ArrayList<>(terms);
            for (int i = 0; i < conditions.size(); i++) {
                ConditionWithFactory conditionWithFactory = findKeyValueCondition(conditions.get(i), filter);
                if (conditionWithFactory != null) {
                    conditions.remove(i);
                    return new ConditionWithFactory(conditionWithFactory.key(),
                            conditionWithFactory.value(),
                            condition1 -> {
                                conditions.add(conditionWithFactory.factory().apply(condition1));
                                return new CombinedCondition(CombinedCondition.Operation.OR, conditions);
                            });
                }
            }
        }

        return null;
    }

    private static Map<EightWayDirection, Condition> rotateCenterConditions() {

        Map<EightWayDirection, Condition> conditions = new HashMap<>();
        for (EightWayDirection direction : EightWayDirection.getCardinalDirections()) {

            // we just need this once per axis
            if (direction.getX() == 1 || direction.getZ() == 1) {

                EightWayDirection interDirection = direction.rotateClockWise();
                Condition newCondition = getAndCondition(interDirection, interDirection.getOpposite());
                conditions.put(direction, newCondition);
            }
        }

        return conditions;
    }

    private static Condition negateCondition(Condition condition) {
        return new Condition() {
            @Override
            public <O, S extends StateHolder<O, S>> Predicate<S> instantiate(StateDefinition<O, S> stateDefinition) {
                return condition.instantiate(stateDefinition).negate();
            }
        };
    }

    private static Condition getAndCondition(EightWayDirection... directions) {
        ConditionBuilder conditionBuilder = new ConditionBuilder();
        for (EightWayDirection direction : EightWayDirection.values()) {
            BooleanProperty property = StarCollisionBlock.PROPERTY_BY_DIRECTION.get(direction);
            Boolean value = ArrayUtils.contains(directions, direction) ? Boolean.TRUE : Boolean.FALSE;
            conditionBuilder.term(property, value);
        }
        return conditionBuilder.build();
    }

    public static KeyValueCondition createKeyValueCondition(String key, String value) {
        return new KeyValueCondition(Collections.singletonMap(key,
                new KeyValueCondition.Terms(Collections.singletonList(new KeyValueCondition.Term(value, false)))));
    }

    private static void appendNewSelector(Condition condition, Selector selector, EightWayDirection direction, List<Selector> newSelectors) {
        appendNewSelector(condition, selector.variant(), direction.toDirection(), newSelectors::add);
    }

    private static void appendNewSelector(Condition condition, BlockStateModel.Unbaked variant, Direction direction, Consumer<Selector> newSelectors) {
        newSelectors.accept(new Selector(Optional.of(condition), new BlockStateModel.Unbaked() {
            @Override
            public void resolveDependencies(Resolver resolver) {
                variant.resolveDependencies(resolver);
            }

            @Override
            public BlockStateModel bake(ModelBaker modelBaker) {
                BlockStateModel blockStateModel = variant.bake(modelBaker);
                Function<BlockModelPart, BlockModelPart> blockModelPartRotator = Util.memoize((BlockModelPart blockModelPart) -> {
                    return MultipartAppender.rotateMultipartSegment(blockModelPart, direction);
                });
                return new BlockStateModel() {
                    @Override
                    public void collectParts(RandomSource randomSource, List<BlockModelPart> list) {
                        List<BlockModelPart> tmpList = new ObjectArrayList<>();
                        blockStateModel.collectParts(randomSource, tmpList);
                        for (BlockModelPart blockModelPart : tmpList) {
                            list.add(blockModelPartRotator.apply(blockModelPart));
                        }
                    }

                    @Override
                    public TextureAtlasSprite particleIcon() {
                        return blockStateModel.particleIcon();
                    }
                };
            }
        }));
    }

    /**
     * Duplicate and rotate all {@link BakedQuad quads} from the given {@link BakedModel segmentModel} to produce a new
     * variant for a diagonal segment
     */
    public static BlockModelPart rotateMultipartSegment(BlockModelPart segmentModel, Direction armDir) {

        Map<Direction, List<BakedQuad>> quadMap = new HashMap<>();
        rotateQuads(quadMap, segmentModel, null, armDir);

        for (Direction cullFace : Direction.values()) {
            rotateQuads(quadMap, segmentModel, cullFace, armDir);
        }

        QuadCollection.Builder builder = new QuadCollection.Builder();
        for (BakedQuad bakedQuad : quadMap.getOrDefault(null, Collections.emptyList())) {
            builder.addUnculledFace(bakedQuad);
        }
        quadMap.forEach((direction, bakedQuads) -> {
            for (BakedQuad bakedQuad : bakedQuads) {
                builder.addCulledFace(direction, bakedQuad);
            }
        });

        return new SimpleModelWrapper(builder.build(), segmentModel.useAmbientOcclusion(), segmentModel.particleIcon());
    }

    /**
     * Rotate all {@link BakedQuad}s with the given {@link Direction cullFace} of the given
     * {@link BakedModel segmentModel} 45 degrees clockwise. The quads are duplicated, rotated and have their vertex
     * normals recalculated.
     */
    private static void rotateQuads(Map<Direction, List<BakedQuad>> quadMap, BlockModelPart blockModelPart, Direction cullFace, Direction segmentDir) {

        List<BakedQuad> quads = blockModelPart.getQuads(cullFace);
        List<BakedQuad> newQuads = new ArrayList<>();

        for (BakedQuad bakedQuad : quads) {
            BakedQuad copy = QuadUtils.copy(bakedQuad);
            rotateQuad(copy, segmentDir);
            newQuads.add(copy);
        }

        quadMap.put(cullFace, newQuads);
    }

    /**
     * Rotate the given {@link BakedQuad quad} 45 degree clockwise and recalculate its vertex normals
     *
     * @param bakedQuad The given BakedQuad, must be a deep-copy of the original
     * @param direction The {@link Direction dir} in which the fence/wall arm this quad belongs to points
     */
    private static void rotateQuad(BakedQuad bakedQuad, Direction direction) {

        Vector3f scaleMult = new Vector3f(Math.abs(direction.getStepX()), 1, Math.abs(direction.getStepZ()));

        Vector3f scaleVec = new Vector3f(1.0F, 0.0F, 1.0F);
        scaleVec.mul(SCALE_ROTATION_45);
        scaleVec.mul(scaleMult.x(), scaleMult.y(), scaleMult.z());
        scaleVec.add(1.0F, 1.0F, 1.0F);

        for (int i = 0; i < 4; i++) {

            Vector4f vector4f = new Vector4f(QuadUtils.getX(bakedQuad, i) - ROTATION_ORIGIN.x(),
                    QuadUtils.getY(bakedQuad, i) - ROTATION_ORIGIN.y(),
                    QuadUtils.getZ(bakedQuad, i) - ROTATION_ORIGIN.z(),
                    1.0F);
            vector4f.mul(new Vector4f(scaleVec, 1.0F));
            ROTATION_MATRIX.transform(vector4f);

            QuadUtils.setPosition(bakedQuad,
                    i,
                    vector4f.x() + ROTATION_ORIGIN.x(),
                    vector4f.y() + ROTATION_ORIGIN.y(),
                    vector4f.z() + ROTATION_ORIGIN.z());
        }

        QuadUtils.fillNormal(bakedQuad);
    }

    private record ConditionWithFactory(String key, String value, UnaryOperator<Condition> factory) {

    }
}
