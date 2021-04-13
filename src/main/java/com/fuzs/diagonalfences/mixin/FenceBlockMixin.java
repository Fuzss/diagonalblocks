package com.fuzs.diagonalfences.mixin;

import com.fuzs.diagonalfences.DiagonalFences;
import com.fuzs.diagonalfences.util.EightWayDirection;
import com.fuzs.diagonalfences.block.IEightWayBlock;
import com.fuzs.diagonalfences.util.shape.NoneVoxelShape;
import com.fuzs.diagonalfences.util.shape.VoxelCollection;
import com.fuzs.diagonalfences.util.shape.VoxelUtils;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
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
import net.minecraft.util.math.shapes.VoxelShapes;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.math.vector.Vector3i;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.IWorld;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import java.util.stream.Stream;

@Mixin(FenceBlock.class)
public abstract class FenceBlockMixin extends FourWayBlock implements IEightWayBlock {

    private static final Map<List<Float>, VoxelShape[]> DIMENSIONS_TO_SHAPE_MAP = Maps.newHashMap();
    private Object2IntMap<BlockState> statePaletteMap = new Object2IntOpenHashMap<>();

    public FenceBlockMixin(float nodeWidth, float extensionWidth, float nodeHeight, float extensionHeight, float collisionY, Properties properties) {

        super(nodeWidth, extensionWidth, nodeHeight, extensionHeight, collisionY, properties);
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    public void init(AbstractBlock.Properties properties, CallbackInfo callbackInfo) {

        this.setDefaultState(this.getDefaultState().with(NORTH_EAST, Boolean.FALSE).with(SOUTH_EAST, Boolean.FALSE).with(SOUTH_WEST, Boolean.FALSE).with(NORTH_WEST, Boolean.FALSE));
    }

    @Inject(method = "fillStateContainer", at = @At("TAIL"))
    protected void fillStateContainer(StateContainer.Builder<Block, BlockState> builder, CallbackInfo callbackInfo) {

        this.fillEightWayStateContainer(builder);
    }

    @SuppressWarnings("NullableProblems")
    @Override
    protected int getIndex(BlockState state) {

        if (this.statePaletteMap == null) {

            this.statePaletteMap = new Object2IntOpenHashMap<>();
        }

        return this.statePaletteMap.computeIntIfAbsent(state, this::getEightWayStateIndex);
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



    @SuppressWarnings("NullableProblems")
    @Override
    protected VoxelShape[] makeShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {

        ArrayList<Float> dimensions = Lists.newArrayList(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
        return DIMENSIONS_TO_SHAPE_MAP.computeIfAbsent(dimensions, dimension -> this.makeDiagonalShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight));
    }

    private VoxelCollection[] makeDiagonalShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {

        float nodeStart = 8.0F - nodeWidth;
        float nodeEnd = 8.0F + nodeWidth;
        float extensionStart = 8.0F - extensionWidth;
        float extensionEnd = 8.0F + extensionWidth;

        VoxelShape nodeShape = Block.makeCuboidShape(nodeStart, 0.0, nodeStart, nodeEnd, nodeHeight, nodeEnd);
        Vector3d[] sideShape = new Vector3d[]{new Vector3d(extensionStart, extensionBottom, 0.0), new Vector3d(extensionEnd, extensionHeight, extensionStart)};
        VoxelShape[] verticalShapes = Stream.of(EightWayDirection.values()).limit(4).map(direction -> direction.transform(sideShape)).map(VoxelUtils::makeCuboidShape).toArray(VoxelShape[]::new);
        VoxelShape[] diagonalShapes = Stream.of(EightWayDirection.values()).skip(4).map(direction -> this.getDiagonalShape(extensionWidth, extensionBottom, extensionHeight, direction)).toArray(VoxelShape[]::new);
        VoxelShape[] sideShapes = new VoxelShape[]{verticalShapes[2], verticalShapes[3], verticalShapes[0], verticalShapes[1], diagonalShapes[2], diagonalShapes[3], diagonalShapes[0], diagonalShapes[1]};

        return this.constructStateShapes(nodeShape, sideShapes);
    }

    private VoxelCollection[] constructStateShapes(VoxelShape nodeShape, VoxelShape[] directionalShapes) {

        VoxelCollection[] stateShapes = new VoxelCollection[(int) Math.pow(2, directionalShapes.length)];
        for (int i = 0; i < stateShapes.length; i++) {

            stateShapes[i] = new VoxelCollection(nodeShape);
            for (int j = 0; j < directionalShapes.length; j++) {

                if ((i & (1 << j)) != 0) {

                    stateShapes[i].addVoxelShape(directionalShapes[j]);
                }
            }
        }

        return stateShapes;
    }

    private VoxelShape getDiagonalShape(float extensionWidth, float extensionBottom, float extensionHeight, EightWayDirection direction) {

        VoxelShape collisionShape = this.getDiagonalCollisionShape(extensionWidth, extensionBottom, extensionHeight, direction);
        // adept width for diagonal rotation
        extensionWidth = (float) Math.sqrt(extensionWidth * extensionWidth * 2);
        final float diagonalSide = 0.7071067812F * extensionWidth;
        Vector3d[] corners = VoxelUtils.createVectorArray(-diagonalSide, extensionHeight, diagonalSide, -diagonalSide + 8.0F, extensionHeight, diagonalSide + 8.0F, -diagonalSide, extensionBottom, diagonalSide, -diagonalSide + 8.0F, extensionBottom, diagonalSide + 8.0F, diagonalSide, extensionHeight, -diagonalSide, diagonalSide + 8.0F, extensionHeight, -diagonalSide + 8.0F, diagonalSide, extensionBottom, -diagonalSide, diagonalSide + 8.0F, extensionBottom, -diagonalSide + 8.0F);
        Vector3d[] edges = VoxelUtils.create12Edges(corners);
        if (direction.getDirectionVec().getX() != 1) {

            edges = VoxelUtils.flipX(edges);
        }

        if (direction.getDirectionVec().getZ() != 1) {

            edges = VoxelUtils.flipZ(edges);
        }

        return new NoneVoxelShape(collisionShape, VoxelUtils.scaleDown(edges));
    }

    private VoxelShape getDiagonalCollisionShape(float extensionWidth, float extensionBottom, float extensionHeight, EightWayDirection direction) {

        VoxelShape collisionShape = VoxelShapes.empty();
        for (int i = 0; i < 8; i++) {

            Vector3i directionVec = direction.getDirectionVec();
            int posX = directionVec.getX() > 0 ? i : 16 - i;
            int posZ = directionVec.getZ() > 0 ? i : 16 - i;
            VoxelShape cuboidShape = Block.makeCuboidShape(posX - extensionWidth, extensionBottom, posZ - extensionWidth, posX + extensionWidth, extensionHeight, posZ + extensionWidth);
            collisionShape = VoxelShapes.or(collisionShape, cuboidShape);
        }

        return collisionShape;
    }

    @SuppressWarnings({"NullableProblems", "deprecation"})
    @Override
    public void updateDiagonalNeighbors(BlockState state, IWorld world, BlockPos pos, int flags, int recursionLeft) {

        this.updateDiagonalNeighbors(world, pos, flags, recursionLeft);
    }

    @Inject(method = "updatePostPlacement", at = @At("TAIL"), cancellable = true)
    public void updatePostPlacement(BlockState stateIn, Direction facing, BlockState facingState, IWorld worldIn, BlockPos currentPos, BlockPos facingPos, CallbackInfoReturnable<BlockState> callbackInfo) {

        this.updatePostPlacement(facing, worldIn, currentPos, callbackInfo);
    }

}
