package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.block.EightWayBlock;
import fuzs.diagonalfences.init.ModRegistry;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(FenceBlock.class)
public abstract class FenceBlockMixin extends CrossCollisionBlock implements EightWayBlock {
    private boolean hasProperties;
    private Object2IntMap<BlockState> statePaletteMap;

    public FenceBlockMixin(float nodeWidth, float extensionWidth, float nodeHeight, float extensionHeight, float collisionY, Properties properties) {
        super(nodeWidth, extensionWidth, nodeHeight, extensionHeight, collisionY, properties);
    }

    @Override
    protected VoxelShape[] makeShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {
        if (this.hasProperties()) {
            return this.getShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
        }
        return super.makeShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
    }

    @Override
    protected int getAABBIndex(BlockState state) {
        if (this.hasProperties()) {
            // can't do this in constructor injection as an injection is only possible at tail, but this method is called prior to that
            if (this.statePaletteMap == null) {
                this.statePaletteMap = new Object2IntOpenHashMap<>();
            }
            return this.statePaletteMap.computeIfAbsent(state, this::makeIndex);
        }
        return super.getAABBIndex(state);
    }

    @Override
    public void updateIndirectNeighbourShapes(BlockState state, LevelAccessor world, BlockPos pos, int flags, int recursionLeft) {
        if (this.canConnectDiagonally()) {
            this.updateIndirectNeighbourShapes2(state, world, pos, flags, recursionLeft);
        }
    }

    @Shadow
    public abstract boolean connectsTo(BlockState state, boolean isSideSolid, Direction direction);

    @Shadow
    private boolean isSameFence(BlockState state) {
        throw new IllegalStateException();
    }

    @Override
    public boolean hasProperties() {
        return this.hasProperties;
    }

    @Override
    public boolean canConnect(BlockGetter blockGetter, BlockPos position, BlockState state, Direction direction) {
        return this.connectsTo(state, state.isFaceSturdy(blockGetter, position, direction), direction);
    }

    @Override
    public boolean canConnectDiagonally() {
        return this.hasProperties() && !this.builtInRegistryHolder().is(ModRegistry.NON_DIAGONAL_FENCES_TAG);
    }

    @Override
    public boolean canConnectDiagonally(BlockState blockstate) {
        return blockstate.getBlock() instanceof FenceBlock && ((EightWayBlock) blockstate.getBlock()).canConnectDiagonally() && this.isSameFence(blockstate);
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    public void diagonalfences_init(BlockBehaviour.Properties properties, CallbackInfo callback) {
        if (this.hasProperties()) {
            // most properties are added in actual constructor
            this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
        }
    }

    @Inject(method = "createBlockStateDefinition", at = @At("TAIL"))
    protected void diagonalfences_createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder, CallbackInfo callback) {
        // do nothing later on when this wasn't called
        this.hasProperties = true;
        this.createBlockStateDefinition2(builder);
    }

    @Inject(method = "getStateForPlacement", at = @At("HEAD"), cancellable = true)
    public void diagonalfences_getStateForPlacement(BlockPlaceContext context, CallbackInfoReturnable<BlockState> callback) {
        if (this.canConnectDiagonally()) {
            BlockGetter iblockreader = context.getLevel();
            BlockPos basePos = context.getClickedPos();
            FluidState fluidState = context.getLevel().getFluidState(context.getClickedPos());
            BlockState placementState = super.getStateForPlacement(context);
            placementState = this.makeStateForPlacement(placementState, iblockreader, basePos, fluidState);
            callback.setReturnValue(placementState);
        }
    }

    @Inject(method = "updateShape", at = @At("TAIL"), cancellable = true)
    public void diagonalfences_updateShape(BlockState state, Direction facing, BlockState facingState, LevelAccessor level, BlockPos currentPos, BlockPos facingPos, CallbackInfoReturnable<BlockState> callback) {
        if (this.canConnectDiagonally()) {
            BlockState returnState = this.updateShape2(state, facing, facingState, level, currentPos, facingPos, callback.getReturnValue());
            if (returnState != null) {
                callback.setReturnValue(returnState);
            }
        }
    }
}
