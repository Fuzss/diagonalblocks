package fuzs.diagonalfences.world.level.block;

import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;

/**
 * An adapter for legacy methods from the old mixin implementation.
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
}
