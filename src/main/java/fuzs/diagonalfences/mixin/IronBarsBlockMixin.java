package fuzs.diagonalfences.mixin;

import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.block.EightWayBlock;
import fuzs.diagonalfences.util.math.shapes.VoxelCollection;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.CrossCollisionBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.phys.shapes.VoxelShape;
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
@Mixin(IronBarsBlock.class)
public abstract class IronBarsBlockMixin extends CrossCollisionBlock implements EightWayBlock {

    private boolean hasProperties;
    private Object2IntMap<BlockState> statePaletteMap;

    public IronBarsBlockMixin(float nodeWidth, float extensionWidth, float nodeHeight, float extensionHeight, float collisionY, Properties properties) {

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
    public abstract boolean attachsTo(BlockState pState, boolean pSolidSide);

    @Override
    public boolean hasProperties() {

        return this.hasProperties;
    }

    @Override
    public boolean canConnect(BlockGetter blockGetter, BlockPos position, BlockState state, Direction direction) {
        return this.attachsTo(state, state.isFaceSturdy(blockGetter, position, direction));
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
        return block instanceof IronBarsBlock && ((EightWayBlock) block).canConnectDiagonally();
    }

    @Override
    public VoxelCollection[] constructStateShapes(VoxelShape nodeShape, VoxelShape[] directionalShapes) {

        VoxelCollection[] stateShapes = new VoxelCollection[(int) Math.pow(2, directionalShapes.length)];
        for (int i = 0; i < stateShapes.length; i++) {

            // don't render outline for node as the texture is transparent making it feel out of place
            if (((i & (1 << 4)) != 0 && (i & (1 << 6)) != 0) || ((i & (1 << 5)) != 0 && (i & (1 << 7)) != 0)) {

                stateShapes[i] = new VoxelCollection();
            } else {

                stateShapes[i] = new VoxelCollection(nodeShape);
            }

            for (int j = 0; j < directionalShapes.length; j++) {

                if ((i & (1 << j)) != 0) {

                    stateShapes[i].addVoxelShape(directionalShapes[j]);
                }
            }
        }

        return stateShapes;
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    public void init$tail(BlockBehaviour.Properties properties, CallbackInfo callback) {

        if (this.hasProperties()) {

            this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
        }
    }

    @Inject(method = "createBlockStateDefinition", at = @At("TAIL"))
    protected void createBlockStateDefinition$tail(StateDefinition.Builder<Block, BlockState> builder, CallbackInfo callback) {

        // do nothing later on when this wasn't called
        this.hasProperties = true;
        this.createBlockStateDefinition2(builder);
    }

    @Inject(method = "getStateForPlacement", at = @At("HEAD"), cancellable = true)
    public void getStateForPlacement$head(BlockPlaceContext context, CallbackInfoReturnable<BlockState> callback) {

        if (this.canConnectDiagonally()) {

            BlockGetter iblockreader = context.getLevel();
            BlockPos basePos = context.getClickedPos();
            FluidState fluidState = context.getLevel().getFluidState(basePos);

            BlockState placementState = super.getStateForPlacement(context);
            placementState = this.makeStateForPlacement(placementState, iblockreader, basePos, fluidState);
            callback.setReturnValue(placementState);
        }
    }

    @Inject(method = "updateShape", at = @At("TAIL"), cancellable = true)
    public void updateShape$tail(BlockState stateIn, Direction facing, BlockState facingState, LevelAccessor worldIn, BlockPos currentPos, BlockPos facingPos, CallbackInfoReturnable<BlockState> callback) {

        if (this.canConnectDiagonally()) {

            BlockState returnState = this.updateShape2(stateIn, facing, facingState, worldIn, currentPos, facingPos, callback.getReturnValue());
            if (returnState != null) {

                callback.setReturnValue(returnState);
            }
        }
    }

}
