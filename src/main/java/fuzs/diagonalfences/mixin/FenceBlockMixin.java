package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.block.IEightWayBlock;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.world.level.block.*;
import net.minecraft.world.level.block.state.*;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.core.Direction;
import net.minecraft.core.BlockPos;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.LevelAccessor;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.tags.ITagManager;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Objects;

@SuppressWarnings("unused")
@Mixin(FenceBlock.class)
public abstract class FenceBlockMixin extends CrossCollisionBlock implements IEightWayBlock {

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

    @SuppressWarnings("deprecation")
    @Override
    public void updateIndirectNeighbourShapes(BlockState state, LevelAccessor world, BlockPos pos, int flags, int recursionLeft) {

        if (this.canConnectDiagonally()) {

            this.updateDiagonalNeighbors2(state, world, pos, flags, recursionLeft);
        }
    }

    @Shadow
    public abstract boolean connectsTo(BlockState state, boolean isSideSolid, Direction direction);

    @Shadow
    private boolean isSameFence(BlockState state) {

        throw new IllegalStateException();
    }

    @Shadow protected abstract void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> p_53334_);

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

        ITagManager<Block> blockTags = Objects.requireNonNull(ForgeRegistries.BLOCKS.tags(), "ITagManager missing for Block registry?!");

        // Unbound tags will gracefully handle queries with an empty collection
        return this.hasProperties() && !blockTags.getTag(DiagonalFences.NON_DIAGONAL_FENCES_TAG).contains(this);
    }

    @Override
    public boolean canConnectDiagonally(BlockState blockstate) {

        Block block = blockstate.getBlock();
        return block instanceof FenceBlock && ((IEightWayBlock) block).canConnectDiagonally() && this.isSameFence(blockstate);
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    public void diagonalfences_init(BlockBehaviour.Properties properties, CallbackInfo callbackInfo) {

        if (this.hasProperties()) {

            // most properties are added in actual constructor
            this.registerDefaultState(this.getDefaultStates(this.defaultBlockState()));
        }
    }

    @Inject(method = "createBlockStateDefinition", at = @At("TAIL"))
    protected void diagonalfences_createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder, CallbackInfo callbackInfo) {

        // do nothing later on when this wasn't called
        this.hasProperties = true;
        this.fillStateContainer2(builder);
    }

    @SuppressWarnings("ConstantConditions")
    @Inject(method = "getStateForPlacement", at = @At("HEAD"), cancellable = true)
    public void diagonalfences_getStateForPlacement(BlockPlaceContext context, CallbackInfoReturnable<BlockState> callbackInfo) {

        if (this.canConnectDiagonally()) {

            BlockGetter iblockreader = context.getLevel();
            BlockPos basePos = context.getClickedPos();
            FluidState fluidState = context.getLevel().getFluidState(context.getClickedPos());

            BlockState placementState = super.getStateForPlacement(context);
            placementState = this.makeStateForPlacement(placementState, iblockreader, basePos, fluidState);
            callbackInfo.setReturnValue(placementState);
        }
    }

    @Inject(method = "updateShape", at = @At("TAIL"), cancellable = true)
    public void diagonalfences_updateShape(BlockState state, Direction facing, BlockState facingState, LevelAccessor level, BlockPos currentPos, BlockPos facingPos, CallbackInfoReturnable<BlockState> callbackInfo) {

        if (this.canConnectDiagonally()) {

            BlockState returnState = this.updatePostPlacement2(state, facing, facingState, level, currentPos, facingPos, callbackInfo.getReturnValue());
            if (returnState != null) {

                callbackInfo.setReturnValue(returnState);
            }
        }
    }

}
