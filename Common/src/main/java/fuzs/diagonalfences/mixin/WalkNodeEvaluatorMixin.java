package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.core.EightWayDirection;
import fuzs.diagonalfences.world.level.block.EightWayBlock;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.pathfinder.Node;
import net.minecraft.world.level.pathfinder.NodeEvaluator;
import net.minecraft.world.level.pathfinder.WalkNodeEvaluator;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.ModifyVariable;

@Mixin(WalkNodeEvaluator.class)
abstract class WalkNodeEvaluatorMixin extends NodeEvaluator {

    @ModifyVariable(method = "isDiagonalValid", at = @At("STORE"))
    protected boolean isDiagonalValid(boolean isDiagonalValid, Node node, @Nullable Node node2, @Nullable Node node3, @Nullable Node node4) {
        // small animals (width less than 0.5, like chicken and rabbits) can walk through unconnected fences diagonally, but we add connections in there
        // so check if a diagonal connection is currently present and if so prevent the animal from trying to walk through
        if (isDiagonalValid && node2 != null && node3 != null && node2.y == node3.y) {
            for (EightWayDirection direction : EightWayDirection.INTERCARDINAL_DIRECTIONS) {
                if (direction.directionVec().getX() == node2.x - node3.x && direction.directionVec().getZ() == node2.z - node3.z) {
                    BlockState state = this.level.getBlockState(new BlockPos(node3.x, node3.y, node3.z));
                    BooleanProperty property = EightWayBlock.DIRECTION_TO_PROPERTY_MAP.get(direction);
                    if (state.hasProperty(property) && state.getValue(property)) {
                        state = this.level.getBlockState(new BlockPos(node2.x, node2.y, node2.z));
                        property = EightWayBlock.DIRECTION_TO_PROPERTY_MAP.get(direction.opposite());
                        if (state.hasProperty(property) && state.getValue(property)) {
                            return false;
                        }
                    }
                }
            }
        }
        return isDiagonalValid;
    }
}
