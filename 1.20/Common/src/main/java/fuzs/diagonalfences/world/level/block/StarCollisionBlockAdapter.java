package fuzs.diagonalfences.world.level.block;

/**
 * An adapter for legacy methods from the old mixin implementation.
 */
public interface StarCollisionBlockAdapter extends StarCollisionBlock {

    @Override
    default boolean hasProperties() {
        return true;
    }

    @Override
    default boolean supportsDiagonalConnections() {
        return true;
    }
}
