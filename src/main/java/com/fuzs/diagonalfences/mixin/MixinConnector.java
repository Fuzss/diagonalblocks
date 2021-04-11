package com.fuzs.diagonalfences.mixin;

import com.fuzs.diagonalfences.DiagonalFences;
import org.spongepowered.asm.mixin.Mixins;
import org.spongepowered.asm.mixin.connect.IMixinConnector;

@SuppressWarnings("unused")
public class MixinConnector implements IMixinConnector {

    @Override
    public void connect() {

        Mixins.addConfiguration("META-INF/" + DiagonalFences.MODID + ".mixins.json");
    }

}
