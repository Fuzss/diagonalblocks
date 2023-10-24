package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.api.v2.DiagonalBlockType;
import fuzs.diagonalfences.api.v2.DiagonalBlockV2;
import fuzs.diagonalfences.world.phys.shapes.VoxelCollection;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

public class DiagonalGlassPaneBlock extends IronBarsBlock implements StarCollisionBlock {
    private Object2IntMap<BlockState> statePaletteMap;
    private final Block block;

    public DiagonalGlassPaneBlock(Block block) {
        super(BlockBehaviour.Properties.copy(block).dropsLike(block));
        this.block = block;
        this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
    }

    @Override
    public String getDescriptionId() {
        return this.block.getDescriptionId();
    }

    @Override
    protected VoxelShape[] makeShapes(float nodeWidth, float extensionWidth, float nodeHeight, float extensionBottom, float extensionHeight) {
        return this.getShapes(nodeWidth, extensionWidth, nodeHeight, extensionBottom, extensionHeight);
    }

    @Override
    protected int getAABBIndex(BlockState state) {
        if (this.statePaletteMap == null) this.statePaletteMap = new Object2IntOpenHashMap<>();
        return this.statePaletteMap.computeIfAbsent(state, this::makeIndex);
    }

    @Override
    public void updateIndirectNeighbourShapes(BlockState blockState, LevelAccessor levelAccessor, BlockPos blockPos, int flags, int recursionLeft) {
        this._updateIndirectNeighbourShapes(blockState, levelAccessor, blockPos, flags, recursionLeft);
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        super.createBlockStateDefinition(builder);
        this._createBlockStateDefinition(builder);
    }

    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return this._getStateForPlacement(context, super.getStateForPlacement(context));
    }

    @Override
    public BlockState updateShape(BlockState blockState, Direction direction, BlockState neighboringBlockState, LevelAccessor levelAccessor, BlockPos blockPos, BlockPos neighboringBlockPos) {
        blockState = super.updateShape(blockState, direction, neighboringBlockState, levelAccessor, blockPos, neighboringBlockPos);
        return this._updateShape(blockState, direction, neighboringBlockState, levelAccessor, blockPos, neighboringBlockPos);
    }

    @Override
    public DiagonalBlockType getType() {
        return DiagonalBlockType.WINDOWS;
    }

    @Override
    public boolean attachesDirectlyTo(BlockState blockState, boolean isSideSolid, Direction direction) {
        return this.attachsTo(blockState, isSideSolid);
    }

    @Override
    public boolean attachesDiagonallyTo(BlockState blockState) {
        return StarCollisionBlock.super.attachesDiagonallyTo(blockState) || blockState.getBlock() instanceof DiagonalBlockV2 diagonalBlock && diagonalBlock.getType() == DiagonalBlockType.WALLS;
    }

    @Override
    public VoxelCollection[] constructStateShapes(VoxelShape nodeShape, VoxelShape[] directionalShapes, VoxelShape[] particleDirectionalShapes) {
        VoxelCollection[] stateShapes = new VoxelCollection[(int) Math.pow(2, directionalShapes.length)];
        for (int i = 0; i < stateShapes.length; i++) {
            // don't render outline for node as the texture is transparent making it feel out of place
            if (((i & (1 << 4)) != 0 && (i & (1 << 6)) != 0) || ((i & (1 << 5)) != 0 && (i & (1 << 7)) != 0)) {
                stateShapes[i] = new VoxelCollection(nodeShape, Shapes.empty());
            } else {
                stateShapes[i] = new VoxelCollection(nodeShape);
            }
            for (int j = 0; j < directionalShapes.length; j++) {
                if ((i & (1 << j)) != 0) {
                    stateShapes[i].addVoxelShape(directionalShapes[j], particleDirectionalShapes[j]);
                }
            }
        }
        return stateShapes;
    }
}
