package fuzs.diagonalblocks.impl.client.resources.model;

import com.google.common.collect.Lists;
import fuzs.diagonalblocks.api.v2.util.EightWayDirection;
import fuzs.diagonalblocks.api.v2.block.StarCollisionBlock;
import net.minecraft.client.data.models.blockstates.ConditionBuilder;
import net.minecraft.client.renderer.block.model.BlockModelDefinition;
import net.minecraft.client.renderer.block.model.BlockStateModel;
import net.minecraft.client.renderer.block.model.multipart.CombinedCondition;
import net.minecraft.client.renderer.block.model.multipart.Condition;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableObject;

import java.util.*;
import java.util.function.Consumer;

public class MultiPartAppender {

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
            Condition oldCondition = selector.condition().orElse(null);

            if (oldCondition != null) {

                MutableObject<EightWayDirection> directionHolder = new MutableObject<>();
                MutableBoolean isTrueHolder = new MutableBoolean();
                Condition newCondition = ConditionHelper.deepCopy(oldCondition, (String key) -> {
                    EightWayDirection direction = EightWayDirection.byName(key);
                    if (direction != null) {
                        if (directionHolder.getValue() == null) {
                            directionHolder.setValue(direction);
                        }
                        return direction.rotateClockWise().toString();
                    }
                    return key;
                }, (String key, String value) -> {
                    if (isTrueHolder.isFalse() && EightWayDirection.byName(key) != null) {
                        if (Objects.equals(value, Boolean.TRUE.toString())) {
                            isTrueHolder.setTrue();
                        }
                    }
                    return value;
                });

                EightWayDirection direction = directionHolder.getValue();
                if (direction != null) {

                    // intercardinal selectors are already present, either a resource pack has dedicated support for diagonal fences or the model baking event has run multiple times
                    // either way stop any processing of selectors, no new selectors have been added as that's done at the end of the method
                    if (direction.isIntercardinal()) {
                        return multiPart;
                    } else if (isTrueHolder.isTrue()) {
                        appendNewSelector(newCondition, selector, direction, newSelectors);
                    } else {

                        // this deals with the model part that shows on the side of the center post of glass panes when the model part corresponding to that direction is NOT being rendered
                        // we adjust the condition so it no longer renders when only two opposite intercardinal directions are present, since we handle that with our own model parts
                        Condition newCondition22 = ConditionHelper.negate(new CombinedCondition(CombinedCondition.Operation.OR,
                                List.copyOf(rotateCenterConditions().values())));
                        Selector newSelector = new Selector(Optional.of(new CombinedCondition(CombinedCondition.Operation.AND,
                                Lists.newArrayList(oldCondition, newCondition22))), selector.variant());
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
            } else if (rotateCenter) {

                // this is the center post, we only use this for glass panes
                // it is rotated and used as a separate model part when only two opposite intercardinal directions are present
                // otherwise it renders normally, but we also change the vanilla condition to exclude the two cases that can result from our new model parts
                Map<EightWayDirection, Condition> conditions = rotateCenterConditions();
                for (Map.Entry<EightWayDirection, Condition> entry : conditions.entrySet()) {

                    appendNewSelector(entry.getValue(), selector, entry.getKey(), newSelectors);
                }

                Selector newSelector = new Selector(Optional.of(ConditionHelper.negate(new CombinedCondition(CombinedCondition.Operation.OR,
                        List.copyOf(conditions.values())))), selector.variant());
                iterator.set(newSelector);
            }
        }

        selectors.addAll(newSelectors);
        return new BlockModelDefinition.MultiPartDefinition(selectors);
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

    private static Condition getAndCondition(EightWayDirection... directions) {
        ConditionBuilder conditionBuilder = new ConditionBuilder();
        for (EightWayDirection direction : EightWayDirection.values()) {
            BooleanProperty property = StarCollisionBlock.PROPERTY_BY_DIRECTION.get(direction);
            Boolean value = ArrayUtils.contains(directions, direction) ? Boolean.TRUE : Boolean.FALSE;
            conditionBuilder.term(property, value);
        }
        return conditionBuilder.build();
    }

    private static void appendNewSelector(Condition condition, Selector selector, EightWayDirection direction, List<Selector> newSelectors) {
        appendNewSelector(condition, selector.variant(), direction.toDirection(), newSelectors::add);
    }

    private static void appendNewSelector(Condition condition, BlockStateModel.Unbaked variant, Direction direction, Consumer<Selector> selectorConsumer) {
        selectorConsumer.accept(new Selector(Optional.of(condition), new RotatedVariant(variant, direction)));
    }
}
