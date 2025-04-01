package fuzs.diagonalblocks.services;

import fuzs.puzzleslib.api.core.v1.ServiceProviderHelper;

public interface ClientAbstractions {
    ClientAbstractions INSTANCE = ServiceProviderHelper.load(ClientAbstractions.class);
}
