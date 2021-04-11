package com.fuzs.diagonalfences.block;

import com.fuzs.diagonalfences.util.EightWayDirection;
import com.google.common.collect.Maps;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.SixWayBlock;
import net.minecraft.state.BooleanProperty;
import net.minecraft.state.StateContainer;
import net.minecraft.util.Direction;
import net.minecraft.util.Util;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3i;
import net.minecraft.world.IWorld;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Map;

public interface IEightWayBlock {

    BooleanProperty NORTH_EAST = BooleanProperty.create("north_east");
    BooleanProperty SOUTH_EAST = BooleanProperty.create("south_east");
    BooleanProperty SOUTH_WEST = BooleanProperty.create("south_west");
    BooleanProperty NORTH_WEST = BooleanProperty.create("north_west");

    Map<EightWayDirection, BooleanProperty> DIRECTION_TO_PROPERTY_MAP = Util.make(Maps.newEnumMap(EightWayDirection.class), (directions) -> {

        directions.put(EightWayDirection.NORTH, SixWayBlock.NORTH);
        directions.put(EightWayDirection.EAST, SixWayBlock.EAST);
        directions.put(EightWayDirection.SOUTH, SixWayBlock.SOUTH);
        directions.put(EightWayDirection.WEST, SixWayBlock.WEST);
        directions.put(EightWayDirection.NORTH_EAST, NORTH_EAST);
        directions.put(EightWayDirection.SOUTH_EAST, SOUTH_EAST);
        directions.put(EightWayDirection.SOUTH_WEST, SOUTH_WEST);
        directions.put(EightWayDirection.NORTH_WEST, NORTH_WEST);
    });

    default void fillEightWayStateContainer(StateContainer.Builder<Block, BlockState> builder) {

        builder.add(NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST);
    }

    default int getEightWayStateIndex(BlockState stateIn) {

        int index = 0;
        for (Map.Entry<EightWayDirection, BooleanProperty> entry : DIRECTION_TO_PROPERTY_MAP.entrySet()) {

            if (stateIn.get(entry.getValue())) {

                index |= entry.getKey().getHorizontalIndex();
            }
        }

        return index;
    }

    boolean canConnectDiagonally(BlockState blockstate);

    default void updatePostPlacement(Direction facing, IWorld worldIn, BlockPos currentPos, CallbackInfoReturnable<BlockState> callbackInfo) {

        if (facing.getAxis().getPlane() == Direction.Plane.HORIZONTAL) {

            BlockState returnState = callbackInfo.getReturnValue();
            for (EightWayDirection direction : EightWayDirection.convert(facing).getIntercardinals()) {

                BlockPos pos = currentPos.add(direction.getDirectionVec());
                BlockState diagonalState = worldIn.getBlockState(pos);
                boolean isBlocked = false;
                for (EightWayDirection cardinal : direction.getCardinals()) {

                    isBlocked = isBlocked || returnState.get(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                returnState = returnState.with(DIRECTION_TO_PROPERTY_MAP.get(direction), !isBlocked && this.canConnectDiagonally(diagonalState));
            }

            callbackInfo.setReturnValue(returnState);
        }
    }

    default void updateDiagonalNeighbors(IWorld world, BlockPos pos, int flags, int recursionLeft) {

        BlockPos.Mutable diagonalPos = new BlockPos.Mutable();
        for (EightWayDirection direction : EightWayDirection.getAllIntercardinals()) {

            Vector3i directionVec = direction.getDirectionVec();
            diagonalPos.setAndOffset(pos, directionVec.getX(), directionVec.getY(), directionVec.getZ());
            BlockState diagonalState = world.getBlockState(diagonalPos);
            if (diagonalState.getBlock() instanceof IEightWayBlock) {

                boolean isBlocked = false;
                for (EightWayDirection cardinal : direction.opposite().getCardinals()) {

                    isBlocked = isBlocked || diagonalState.get(DIRECTION_TO_PROPERTY_MAP.get(cardinal));
                }

                BlockState diagonalStateUpdated = diagonalState.with(DIRECTION_TO_PROPERTY_MAP.get(direction.opposite()), !isBlocked && ((IEightWayBlock) diagonalState.getBlock()).canConnectDiagonally(world.getBlockState(pos)));
                Block.replaceBlockState(diagonalState, diagonalStateUpdated, world, diagonalPos, flags, recursionLeft);
            }
        }
    }

}
