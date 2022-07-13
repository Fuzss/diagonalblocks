package fuzs.diagonalfences.client.model;

import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;

import java.util.function.Predicate;

public record MultipartDirectionData(BlockState testState, Predicate<BlockState> selector, Direction sourceDirection) {

    public static MultipartDirectionData of(BlockState testState, Predicate<BlockState> selector, Direction sourceDirection) {
        return new MultipartDirectionData(testState, selector, sourceDirection);
    }
}