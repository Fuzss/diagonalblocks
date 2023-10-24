package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.api.v2.EightWayDirection;
import net.minecraft.world.level.block.state.BlockState;

/**
 * An adapter for legacy methods from the old mixin implementation to maintain some level of api compatibility.
 */
public interface LegacyDiagonalBlockAdapter extends DiagonalBlock {

    @Deprecated
    @Override
    default boolean hasProperties() {
        return true;
    }

    @Deprecated
    @Override
    default boolean supportsDiagonalConnections() {
        return true;
    }

    @Deprecated
    @Override
    default boolean canConnectToMe(BlockState neighborState, EightWayDirection neighborDirectionToMe) {
        return false;
    }
}
