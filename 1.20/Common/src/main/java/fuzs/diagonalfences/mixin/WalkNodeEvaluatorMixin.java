package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.api.v2.EightWayDirection;
import fuzs.diagonalfences.api.v2.impl.StarCollisionBlock;
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
            for (EightWayDirection direction : EightWayDirection.getIntercardinalDirections()) {
                if (direction.getX() == node2.x - node3.x && direction.getZ() == node2.z - node3.z) {
                    BlockState state = this.level.getBlockState(new BlockPos(node3.x, node3.y, node3.z));
                    BooleanProperty property = StarCollisionBlock.PROPERTY_BY_DIRECTION.get(direction);
                    if (state.hasProperty(property) && state.getValue(property)) {
                        state = this.level.getBlockState(new BlockPos(node2.x, node2.y, node2.z));
                        property = StarCollisionBlock.PROPERTY_BY_DIRECTION.get(direction.getOpposite());
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
