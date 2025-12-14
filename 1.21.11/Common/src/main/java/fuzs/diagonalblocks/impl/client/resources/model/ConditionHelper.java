package fuzs.diagonalblocks.impl.client.resources.model;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import fuzs.diagonalblocks.impl.DiagonalBlocks;
import net.minecraft.client.renderer.block.model.multipart.CombinedCondition;
import net.minecraft.client.renderer.block.model.multipart.Condition;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.StateHolder;
import org.jspecify.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.function.BinaryOperator;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

public final class ConditionHelper {

    private ConditionHelper() {
        // NO-OP
    }

    /**
     * Will return {@code null} when any of the transformers return {@code null} for the string, instead of returning an
     * empty condition.
     */
    @Nullable public static Condition deepCopy(Condition condition, UnaryOperator<String> keyTransformer, BinaryOperator<String> valueTransformer) {
        if (condition instanceof KeyValueCondition(Map<String, KeyValueCondition.Terms> tests)) {
            ImmutableMap.Builder<String, KeyValueCondition.Terms> stringTermsBuilder = ImmutableMap.builder();
            for (Map.Entry<String, KeyValueCondition.Terms> entry : tests.entrySet()) {
                String newKey = keyTransformer.apply(entry.getKey());
                if (newKey != null) {
                    ImmutableList.Builder<KeyValueCondition.Term> termsBuilder = ImmutableList.builder();
                    for (KeyValueCondition.Term term : entry.getValue().entries()) {
                        String newValue = valueTransformer.apply(entry.getKey(), term.value());
                        if (newValue != null) {
                            termsBuilder.add(new KeyValueCondition.Term(newValue, term.negated()));
                        }
                    }
                    ImmutableList<KeyValueCondition.Term> newEntries = termsBuilder.build();
                    if (!newEntries.isEmpty()) {
                        stringTermsBuilder.put(newKey, new KeyValueCondition.Terms(newEntries));
                    }
                }
            }
            ImmutableMap<String, KeyValueCondition.Terms> newTests = stringTermsBuilder.build();
            return newTests.isEmpty() ? null : new KeyValueCondition(newTests);
        } else if (condition instanceof CombinedCondition(
                CombinedCondition.Operation operation, List<Condition> terms
        )) {
            ImmutableList.Builder<Condition> termsBuilder = ImmutableList.builder();
            for (Condition term : terms) {
                Condition newCondition = deepCopy(term, keyTransformer, valueTransformer);
                if (newCondition != null) {
                    termsBuilder.add(newCondition);
                }
            }
            ImmutableList<Condition> newTerms = termsBuilder.build();
            return newTerms.isEmpty() ? null : new CombinedCondition(operation, newTerms);
        } else {
            DiagonalBlocks.LOGGER.warn("Unrecognized condition: {}", condition);
            return condition;
        }
    }

    public static Condition negate(Condition condition) {
        return new Condition() {
            @Override
            public <O, S extends StateHolder<O, S>> Predicate<S> instantiate(StateDefinition<O, S> stateDefinition) {
                return condition.instantiate(stateDefinition).negate();
            }
        };
    }
}
