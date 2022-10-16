package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.core.EightWayDirection;
import fuzs.diagonalfences.init.ModRegistry;
import fuzs.diagonalfences.world.level.block.StarCollisionBlock;
import fuzs.diagonalfences.world.phys.shapes.VoxelCollection;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.tags.BlockTags;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceGateBlock;
import net.minecraft.world.level.block.HorizontalDirectionalBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(FenceGateBlock.class)
abstract class FenceGateBlockMixin extends HorizontalDirectionalBlock implements DiagonalBlock {
    @Shadow
    @Final
    private static VoxelShape Z_SHAPE;
    @Shadow
    @Final
    private static VoxelShape X_SHAPE;
    @Shadow
    @Final
    private static VoxelShape Z_SHAPE_LOW;
    @Shadow
    @Final
    private static VoxelShape X_SHAPE_LOW;
    @Shadow
    @Final
    private static VoxelShape Z_COLLISION_SHAPE;
    @Shadow
    @Final
    private static VoxelShape X_COLLISION_SHAPE;
    @Unique
    private static final VoxelShape DIAGONAL_Z_SHAPE = createFenceGateShape(16.0F, false);
    @Unique
    private static final VoxelShape DIAGONAL_X_SHAPE = createFenceGateShape(16.0F, true);
    @Unique
    private static final VoxelShape DIAGONAL_Z_SHAPE_LOW = createFenceGateShape(13.0F, false);
    @Unique
    private static final VoxelShape DIAGONAL_X_SHAPE_LOW = createFenceGateShape(13.0F, true);
    @Unique
    private static final VoxelShape DIAGONAL_Z_COLLISION_SHAPE = createFenceGateShape(24.0F, false);
    @Unique
    private static final VoxelShape DIAGONAL_X_COLLISION_SHAPE = createFenceGateShape(24.0F, true);

    private boolean diagonalfences$hasProperties;

    protected FenceGateBlockMixin(Properties properties) {
        super(properties);
    }

    @Unique
    private static VoxelCollection createFenceGateShape(float extensionHeight, boolean xShape) {
        VoxelShape collisionShape = StarCollisionBlock.getDiagonalShape(2.0F, 0.0F, extensionHeight, xShape ? EightWayDirection.NORTH_EAST : EightWayDirection.NORTH_WEST, true, 16);
        VoxelShape particleShape = StarCollisionBlock.getDiagonalShape(2.0F, 0.0F, extensionHeight, xShape ? EightWayDirection.NORTH_EAST : EightWayDirection.NORTH_WEST, true, 2);
        VoxelCollection voxelCollection = new VoxelCollection();
        voxelCollection.addVoxelShape(collisionShape, particleShape);
        voxelCollection.finish();
        return voxelCollection;
    }

    @Override
    public boolean hasProperties() {
        return this.diagonalfences$hasProperties;
    }

    @Override
    public boolean supportsDiagonalConnections() {
        return this.hasProperties() && !this.builtInRegistryHolder().is(ModRegistry.NON_DIAGONAL_FENCE_GATES_TAG);
    }

    @Override
    public boolean canConnectToMe(BlockState neighborState, EightWayDirection neighborDirectionToMe) {
        return neighborState.getBlock() instanceof FenceGateBlock && ((DiagonalBlock) neighborState.getBlock()).supportsDiagonalConnections();
    }

    @Inject(method = "getShape", at = @At("HEAD"), cancellable = true)
    public void diagonalfences$getShape(BlockState state, BlockGetter level, BlockPos pos, CollisionContext context, CallbackInfoReturnable<VoxelShape> callback) {
        if (state.getValue(FenceGateBlock.IN_WALL)) {
            VoxelShape shape = switch (state.getValue(DiagonalBlock.FACING2)) {
                case NORTH, SOUTH -> Z_SHAPE_LOW;
                case EAST, WEST -> X_SHAPE_LOW;
                case NORTH_EAST, SOUTH_WEST -> DIAGONAL_Z_SHAPE_LOW;
                case NORTH_WEST, SOUTH_EAST -> DIAGONAL_X_SHAPE_LOW;
            };
            callback.setReturnValue(shape);
        } else {
            VoxelShape shape = switch (state.getValue(DiagonalBlock.FACING2)) {
                case NORTH, SOUTH -> Z_SHAPE;
                case EAST, WEST -> X_SHAPE;
                case NORTH_EAST, SOUTH_WEST -> DIAGONAL_Z_SHAPE;
                case NORTH_WEST, SOUTH_EAST -> DIAGONAL_X_SHAPE;
            };
            callback.setReturnValue(shape);
        }
    }

    @Inject(method = "getCollisionShape", at = @At("HEAD"), cancellable = true)
    public void diagonalfences$getCollisionShape(BlockState state, BlockGetter level, BlockPos pos, CollisionContext context, CallbackInfoReturnable<VoxelShape> callbackInfo) {
        if (!state.getValue(FenceGateBlock.OPEN)) {
            VoxelShape collisionShape = switch (state.getValue(DiagonalBlock.FACING2)) {
                case NORTH, SOUTH -> Z_COLLISION_SHAPE;
                case EAST, WEST -> X_COLLISION_SHAPE;
                case NORTH_EAST, SOUTH_WEST -> DIAGONAL_Z_COLLISION_SHAPE;
                case NORTH_WEST, SOUTH_EAST -> DIAGONAL_X_COLLISION_SHAPE;
            };
            callbackInfo.setReturnValue(collisionShape);
        }
    }

    @Inject(method = "getStateForPlacement", at = @At("TAIL"), cancellable = true)
    public void diagonalfences$getStateForPlacement(BlockPlaceContext context, CallbackInfoReturnable<BlockState> callback) {
        Level level = context.getLevel();
        BlockPos blockPos = context.getClickedPos();
        Direction direction = context.getHorizontalDirection();
        Direction.Axis axis = direction.getAxis();
        if (axis == Direction.Axis.Z && (!this.canConnectToMe(level.getBlockState(blockPos.west())) && !this.canConnectToMe(level.getBlockState(blockPos.east())))) {
            if (this.canConnectToMe(level.getBlockState(blockPos.north())) || this.canConnectToMe(level.getBlockState(blockPos.south()))) {
                direction = direction.getClockWise();
            } else {
                for (EightWayDirection intercardinalDirection : EightWayDirection.getIntercardinalDirections()) {
                    BlockState diagonalState = level.getBlockState(intercardinalDirection.offset(blockPos));
//                    if (diagonalState.getBlock() instanceof DiagonalBlock)
                }
            }
        }
        if (axis == Direction.Axis.X && (!this.canConnectToMe(level.getBlockState(blockPos.north())) && !this.canConnectToMe(level.getBlockState(blockPos.south())))) {
            if (this.canConnectToMe(level.getBlockState(blockPos.west())) && this.canConnectToMe(level.getBlockState(blockPos.east()))) {
                direction = direction.getClockWise();
            }
        }
        BlockState placementState = callback.getReturnValue().setValue(FACING, direction);
        EightWayDirection eightWayDirection = EightWayDirection.toEightWayDirection(direction);
        placementState = placementState.setValue(DiagonalBlock.FACING2, eightWayDirection);
        callback.setReturnValue(placementState);
    }

    @Inject(method = "getOcclusionShape", at = @At("HEAD"), cancellable = true)
    public void diagonalfences$getOcclusionShape(BlockState state, BlockGetter level, BlockPos pos, CallbackInfoReturnable<VoxelShape> callback) {
        if (state.getValue(FACING2).intercardinal()) callback.setReturnValue(Shapes.empty());
    }

    @Inject(method = "createBlockStateDefinition", at = @At("TAIL"))
    protected void diagonalfences$createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder, CallbackInfo callback) {
        // do nothing later on when this wasn't called
        this.diagonalfences$hasProperties = true;
        builder.add(DiagonalBlock.FACING2);
    }

    private boolean canConnectToMe(BlockState state) {
        return state.is(BlockTags.WALLS) || state.is(BlockTags.FENCES);
    }

//    private boolean isHeldInPlace()


    @Override
    public boolean canConnectToNeighbor(BlockState myState, BlockState neighborState, EightWayDirection directionToNeighbor) {
        if (!(myState.getBlock() instanceof DiagonalBlock myBlock) || !myBlock.supportsDiagonalConnections()) return false;
        return myState.getValue(FACING2).intercardinal();
    }
}
