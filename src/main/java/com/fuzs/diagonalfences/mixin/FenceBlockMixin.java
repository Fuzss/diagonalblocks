package com.fuzs.diagonalfences.mixin;

import com.fuzs.diagonalfences.block.IEightWayBlock;
import com.fuzs.diagonalfences.util.EightWayDirection;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.block.*;
import net.minecraft.fluid.FluidState;
import net.minecraft.fluid.Fluids;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.state.StateContainer;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.IWorld;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.stream.Stream;

@SuppressWarnings("unused")
@Mixin(FenceBlock.class)
public abstract class FenceBlockMixin extends FourWayBlock implements IEightWayBlock {

    private Object2IntMap<BlockState> statePaletteMap;

    public FenceBlockMixin(float nodeWidth, float extensionWidth, float nodeHeight, float extensionHeight, float collisionY, Properties properties) {

        super(nodeWidth, extensionWidth, nodeHeight, extensionHeight, collisionY, properties);
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    public void init(AbstractBlock.Properties properties, CallbackInfo callbackInfo) {

        this.setDefaultState(this.getDefaultStates(this.getDefaultState()));
    }

    @Inject(method = "fillStateContainer", at = @At("TAIL"))
    protected void fillStateContainer(StateContainer.Builder<Block, BlockState> builder, CallbackInfo callbackInfo) {

        this.fillStateContainer2(builder);
    }

    @Override
    protected int getIndex(BlockState state) {

        // can't do this in constructor injection as an injection is only possible at tail, but this method is called prior to that
        if (this.statePaletteMap == null) {

            this.statePaletteMap = new Object2IntOpenHashMap<>();
        }

        return this.statePaletteMap.computeIntIfAbsent(state, this::makeIndex);
    }

    @Shadow
    public abstract boolean canConnect(BlockState state, boolean isSideSolid, Direction direction);

    @Override
    public boolean canConnectDiagonally(BlockState blockstate) {

        Block block = blockstate.getBlock();
        return !cannotAttach(block) && this.isWoodenFence(block);
    }

    @Shadow
    private boolean isWoodenFence(Block block) {

        throw new IllegalStateException();
    }

    @SuppressWarnings("ConstantConditions")
    @Inject(method = "getStateForPlacement", at = @At("HEAD"), cancellable = true)
    public void getStateForPlacement(BlockItemUseContext context, CallbackInfoReturnable<BlockState> callbackInfo) {

        IBlockReader iblockreader = context.getWorld();
        BlockPos blockpos = context.getPos();
        FluidState fluidstate = context.getWorld().getFluidState(context.getPos());

        BlockPos[] positions = Stream.of(EightWayDirection.values()).map(EightWayDirection::getDirectionVec).map(blockpos::add).toArray(BlockPos[]::new);
        BlockState[] states = Stream.of(positions).map(iblockreader::getBlockState).toArray(BlockState[]::new);
        int connections = 0;
        for (int i = 0; i < 4; i++) {

            Direction direction = Direction.byHorizontalIndex(i).getOpposite();
            if (this.canConnect(states[i], states[i].isSolidSide(iblockreader, positions[i], direction), direction)) {

                connections |= 1 << i;
            }
        }

        BlockState stateForPlacement = super.getStateForPlacement(context)
                .with(SOUTH, (connections & 1) != 0)
                .with(WEST, (connections & 2) != 0)
                .with(NORTH, (connections & 4) != 0)
                .with(EAST, (connections & 8) != 0)
                .with(SOUTH_WEST, this.canConnectDiagonally(states[4]) && (connections & 1) == 0 && (connections & 2) == 0)
                .with(NORTH_WEST, this.canConnectDiagonally(states[5]) && (connections & 2) == 0 && (connections & 4) == 0)
                .with(NORTH_EAST, this.canConnectDiagonally(states[6]) && (connections & 4) == 0 && (connections & 8) == 0)
                .with(SOUTH_EAST, this.canConnectDiagonally(states[7]) && (connections & 8) == 0 && (connections & 1) == 0)
                .with(WATERLOGGED, fluidstate.getFluid() == Fluids.WATER);

        callbackInfo.setReturnValue(stateForPlacement);
    }

    @Override
    protected VoxelShape[] makeShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {

        return this.getShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
    }

    @Inject(method = "updatePostPlacement", at = @At("TAIL"), cancellable = true)
    public void updatePostPlacement(BlockState stateIn, Direction facing, BlockState facingState, IWorld worldIn, BlockPos currentPos, BlockPos facingPos, CallbackInfoReturnable<BlockState> callbackInfo) {

        BlockState returnState = this.updatePostPlacement2(stateIn, facing, facingState, worldIn, currentPos, facingPos, callbackInfo.getReturnValue());
        if (returnState != null) {

            callbackInfo.setReturnValue(returnState);
        }
    }

    @SuppressWarnings("deprecation")
    @Override
    public void updateDiagonalNeighbors(BlockState state, IWorld world, BlockPos pos, int flags, int recursionLeft) {

        this.updateDiagonalNeighbors2(state, world, pos, flags, recursionLeft);
    }

}
