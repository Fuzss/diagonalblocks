package fuzs.diagonalfences.api;

import net.minecraft.block.BlockState;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IBlockReader;

public interface IDiagonalBlock {

    boolean canConnect(IBlockReader iblockreader, BlockPos position, BlockState state, Direction direction);

    boolean canConnectDiagonally();

    boolean canConnectDiagonally(BlockState blockstate);

}
