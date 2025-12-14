package fuzs.diagonalblocks.api.v2.block;

import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.block.type.DiagonalBlockTypes;
import fuzs.diagonalblocks.api.v2.util.EightWayDirection;
import fuzs.diagonalblocks.impl.world.phys.shapes.VoxelCollection;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.ScheduledTickAccess;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.function.Function;

public class DiagonalGlassPaneBlock extends IronBarsBlock implements StarCollisionBlock {

    public DiagonalGlassPaneBlock(BlockBehaviour.Properties properties) {
        super(properties);
        this.registerDefaultState(this.addDefaultStates(this.defaultBlockState()));
    }

    @Override
    protected Function<BlockState, VoxelShape> makeShapes(float nodeWidth, float nodeHeight, float extensionWidth, float extensionBottom, float extensionHeight) {
        return this._makeShapes(nodeWidth, nodeHeight, extensionWidth, extensionBottom, extensionHeight);
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
    public BlockState updateShape(BlockState blockState, LevelReader levelReader, ScheduledTickAccess scheduledTickAccess, BlockPos blockPos, Direction direction, BlockPos neighboringBlockPos, BlockState neighboringBlockState, RandomSource randomSource) {
        blockState = super.updateShape(blockState,
                levelReader,
                scheduledTickAccess,
                blockPos,
                direction,
                neighboringBlockPos,
                neighboringBlockState,
                randomSource);
        return this._updateShape(blockState,
                levelReader,
                scheduledTickAccess,
                blockPos,
                direction,
                neighboringBlockPos,
                neighboringBlockState,
                randomSource);
    }

    @Override
    public DiagonalBlockType getType() {
        return DiagonalBlockTypes.WINDOW;
    }

    @Override
    public boolean attachesDirectlyTo(BlockState blockState, boolean isSideSolid, Direction direction) {
        return this.attachsTo(blockState, isSideSolid);
    }

    @Override
    public boolean attachesDiagonallyTo(BlockState blockState, EightWayDirection eightWayDirection) {
        return StarCollisionBlock.super.attachesDiagonallyTo(blockState, eightWayDirection)
                || blockState.getBlock() instanceof DiagonalBlock diagonalBlock
                && diagonalBlock.getType() == DiagonalBlockTypes.WALL;
    }

    @Override
    public VoxelShape[] constructStateShapes(VoxelShape nodeShape, VoxelShape[] directionalShapes, VoxelShape[] particleDirectionalShapes) {
        VoxelCollection[] stateShapes = new VoxelCollection[(int) Math.pow(2, directionalShapes.length)];
        for (int i = 0; i < stateShapes.length; i++) {
            VoxelCollection voxelCollection;
            // don't render outline for node as the texture is not visible making it feel out of place
            if (((i & (1 << 4)) != 0 && (i & (1 << 6)) != 0) || ((i & (1 << 5)) != 0 && (i & (1 << 7)) != 0)) {
                voxelCollection = new VoxelCollection(nodeShape, Shapes.empty());
            } else {
                voxelCollection = new VoxelCollection(nodeShape);
            }
            for (int j = 0; j < directionalShapes.length; j++) {
                if ((i & (1 << j)) != 0) {
                    voxelCollection.addVoxelShape(directionalShapes[j], particleDirectionalShapes[j]);
                }
            }
            stateShapes[i] = voxelCollection.optimize();
        }

        return stateShapes;
    }
}
