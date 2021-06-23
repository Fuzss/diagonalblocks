package fuzs.diagonalfences.mixin;

import com.google.common.collect.ImmutableSortedMap;
import com.mojang.serialization.Decoder;
import com.mojang.serialization.Encoder;
import com.mojang.serialization.MapCodec;
import fuzs.diagonalfences.block.IEightWayBlock;
import fuzs.diagonalfences.element.DiagonalFencesElement;
import fuzs.diagonalfences.mixin.accessor.IStateContainerAccessor;
import fuzs.diagonalfences.mixin.accessor.IStateHolderAccessor;
import fuzs.diagonalfences.state.ExposedStateContainerBuilder;
import fuzs.puzzleslib.util.PuzzlesUtil;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.block.*;
import net.minecraft.fluid.FluidState;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.state.Property;
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

import java.util.Map;
import java.util.function.Supplier;
import java.util.stream.Stream;

@SuppressWarnings("unused")
@Mixin(FenceBlock.class)
public abstract class FenceBlockMixin extends FourWayBlock implements IEightWayBlock {

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
    protected int getIndex(BlockState state) {

        if (this.hasProperties()) {

            // can't do this in constructor injection as an injection is only possible at tail, but this method is called prior to that
            if (this.statePaletteMap == null) {

                this.statePaletteMap = new Object2IntOpenHashMap<>();
            }

            return this.statePaletteMap.computeIntIfAbsent(state, this::makeIndex);
        }

        return super.getIndex(state);
    }

    @SuppressWarnings("deprecation")
    @Override
    public void updateDiagonalNeighbors(BlockState state, IWorld world, BlockPos pos, int flags, int recursionLeft) {

        if (this.canConnectDiagonally()) {

            this.updateDiagonalNeighbors2(state, world, pos, flags, recursionLeft);
        }
    }

    @Shadow
    public abstract boolean canConnect(BlockState state, boolean isSideSolid, Direction direction);

    @Shadow
    private boolean isWoodenFence(Block block) {

        throw new IllegalStateException();
    }

    @Override
    public boolean hasProperties() {

        return this.hasProperties;
    }

    @Override
    public boolean canConnect(IBlockReader iblockreader, BlockPos position, BlockState state, Direction direction) {

        return this.canConnect(state, state.isSolidSide(iblockreader, position, direction), direction);
    }

    @Override
    public boolean canConnectDiagonally() {

        try {

            // use this with care as the tag might not have been fetched yet
            return this.hasProperties() && !this.isIn(DiagonalFencesElement.NON_DIAGONAL_FENCES_TAG);
        } catch (Exception ignored) {

        }

        return this.hasProperties();
    }

    @Override
    public boolean canConnectDiagonally(BlockState blockstate) {

        Block block = blockstate.getBlock();
        return block instanceof FenceBlock && ((IEightWayBlock) block).canConnectDiagonally() && this.isWoodenFence(block);
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    public void init(AbstractBlock.Properties properties, CallbackInfo callbackInfo) {

        if (this.hasProperties()) {

            this.setDefaultState(this.getDefaultStates(this.getDefaultState()));

            ExposedStateContainerBuilder<Block, BlockState> builder = PuzzlesUtil.make(new ExposedStateContainerBuilder<>(), this::fillStateContainer);
            ExposedStateContainerBuilder<Block, BlockState> additionalBuilder = PuzzlesUtil.make(new ExposedStateContainerBuilder<>(), this::fillStateContainer2);

            Block owner = this.stateContainer.getOwner();
            Supplier<BlockState> supplier = owner::getDefaultState;
            MapCodec<BlockState> mapcodec = MapCodec.of(Encoder.empty(), Decoder.unit(supplier));

            for (Map.Entry<String, Property<?>> entry : ImmutableSortedMap.copyOf(builder.properties).entrySet()) {

                if (!additionalBuilder.properties.containsKey(entry.getKey())) {

                    mapcodec = IStateContainerAccessor.callSetPropertyCodec(mapcodec, supplier, entry.getKey(), entry.getValue());
                }
            }

            MapCodec<BlockState> mapcodec1 = mapcodec;

            Stream.concat(Stream.of(this.getDefaultState()), this.stateContainer.getValidStates().stream())
                    .map(state -> (IStateHolderAccessor<Block, BlockState>) state)
                    .forEach(state -> state.setCodec(mapcodec1));


//            LenientStateContainer.LenientBuilder<Block, BlockState> builder = new LenientStateContainer.LenientBuilder<>(this);
//            this.fillStateContainer(builder);
//            this.fillAdditionalStateContainer(builder);
//            ((IBlockAccessor) this).setStateContainer(builder.func_235882_a_(Block::getDefaultState, BlockState::new));
//            this.setDefaultState(this.stateContainer.getBaseState());
        }
    }

    @Inject(method = "fillStateContainer", at = @At("TAIL"))
    protected void fillStateContainer(StateContainer.Builder<Block, BlockState> builder, CallbackInfo callbackInfo) {

        // do nothing later on when this wasn't called
        this.hasProperties = true;
        this.fillStateContainer2(builder);
    }

    @SuppressWarnings("ConstantConditions")
    @Inject(method = "getStateForPlacement", at = @At("HEAD"), cancellable = true)
    public void getStateForPlacement(BlockItemUseContext context, CallbackInfoReturnable<BlockState> callbackInfo) {

        if (this.canConnectDiagonally()) {

            IBlockReader iblockreader = context.getWorld();
            BlockPos basePos = context.getPos();
            FluidState fluidState = context.getWorld().getFluidState(context.getPos());

            BlockState placementState = super.getStateForPlacement(context);
            placementState = this.makeStateForPlacement(placementState, iblockreader, basePos, fluidState);
            callbackInfo.setReturnValue(placementState);
        }
    }

    @Inject(method = "updatePostPlacement", at = @At("TAIL"), cancellable = true)
    public void updatePostPlacement(BlockState stateIn, Direction facing, BlockState facingState, IWorld worldIn, BlockPos currentPos, BlockPos facingPos, CallbackInfoReturnable<BlockState> callbackInfo) {

        if (this.canConnectDiagonally()) {

            BlockState returnState = this.updatePostPlacement2(stateIn, facing, facingState, worldIn, currentPos, facingPos, callbackInfo.getReturnValue());
            if (returnState != null) {

                callbackInfo.setReturnValue(returnState);
            }
        }
    }

}
