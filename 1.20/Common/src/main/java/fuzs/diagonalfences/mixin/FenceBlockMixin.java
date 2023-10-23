package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.api.world.level.block.EightWayDirection;
import fuzs.diagonalfences.init.ModRegistry;
import fuzs.diagonalfences.world.level.block.StarCollisionBlock;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(FenceBlock.class)
public abstract class FenceBlockMixin extends CrossCollisionBlock implements StarCollisionBlock {
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
        if (this.supportsDiagonalConnections()) {
            this._updateIndirectNeighbourShapes(state, world, pos, flags, recursionLeft);
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
    public boolean supportsDiagonalConnections() {
        return this.hasProperties() && !this.builtInRegistryHolder().is(ModRegistry.NON_DIAGONAL_FENCES_BLOCK_TAG);
    }

    @Override
    public boolean canConnectToMe(BlockState neighborState, EightWayDirection neighborDirectionToMe) {
        if (neighborState.getBlock() instanceof FenceBlock && ((StarCollisionBlock) neighborState.getBlock()).supportsDiagonalConnections() && this.isSameFence(neighborState)) {
            for (EightWayDirection neighbor : neighborDirectionToMe.getCardinalNeighbors()) {
                if (neighborState.getValue(DIRECTION_TO_PROPERTY_MAP.get(neighbor))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    public void init(BlockBehaviour.Properties properties, CallbackInfo callback) {
        if (this.hasProperties()) {
            // most properties are added in actual constructor
            this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
        }
    }

    @Inject(method = "createBlockStateDefinition", at = @At("TAIL"))
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder, CallbackInfo callback) {
        // do nothing later on when this wasn't called
        this.hasProperties = true;
        this._createBlockStateDefinition(builder);
    }

    @Inject(method = "getStateForPlacement", at = @At("HEAD"), cancellable = true)
    public void getStateForPlacement(BlockPlaceContext context, CallbackInfoReturnable<BlockState> callback) {
        if (this.supportsDiagonalConnections()) {
            callback.setReturnValue(this._getStateForPlacement(context, super.getStateForPlacement(context)));
        }
    }

    @Inject(method = "updateShape", at = @At("TAIL"), cancellable = true)
    public void updateShape(BlockState state, Direction facing, BlockState facingState, LevelAccessor level, BlockPos currentPos, BlockPos facingPos, CallbackInfoReturnable<BlockState> callback) {
        if (this.supportsDiagonalConnections()) {
            callback.setReturnValue(this._updateShape(state, facing, facingState, level, currentPos, facingPos, callback.getReturnValue()));
        }
    }
}
