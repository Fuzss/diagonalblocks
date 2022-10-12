package fuzs.diagonalfences.client.core;

import fuzs.puzzleslib.util.PuzzlesUtil;

public final class ClientModServices {
    public static final ClientAbstractions ABSTRACTIONS = PuzzlesUtil.loadServiceProvider(ClientAbstractions.class);
}
