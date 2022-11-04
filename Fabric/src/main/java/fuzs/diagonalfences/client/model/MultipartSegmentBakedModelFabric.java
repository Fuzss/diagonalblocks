/*
 * Copyright (c) Forge Development LLC and contributors
 * SPDX-License-Identifier: LGPL-2.1-only
 */

package fuzs.diagonalfences.client.model;

import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.ItemOverrides;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.Random;

/**
 * just like {@link net.fabricmc.fabric.api.renderer.v1.model.ForwardingBakedModel} on Fabric and net.minecraftforge.client.model.BakedModelWrapper on Forge
 * we cannot use the Fabric API version as it implements {@link net.fabricmc.fabric.api.renderer.v1.model.FabricBakedModel}
 * which for some odd reason breaks rendering the fences (although it's actually already implemented on {@link BakedModel}) itself via mixins
 */
public class MultipartSegmentBakedModelFabric<T extends BakedModel> implements BakedModel {
    private final T originalModel;
    private final Map<Direction, List<BakedQuad>> quadMap;

    public MultipartSegmentBakedModelFabric(T originalModel, Map<Direction, List<BakedQuad>> quadMap) {
        this.originalModel = originalModel;
        this.quadMap = quadMap;
    }

    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, Random rand) {
        return this.quadMap.get(side);
    }

    @Override
    public boolean useAmbientOcclusion() {
        return this.originalModel.useAmbientOcclusion();
    }

    @Override
    public boolean isGui3d() {
        return this.originalModel.isGui3d();
    }

    @Override
    public boolean usesBlockLight() {
        return this.originalModel.usesBlockLight();
    }

    @Override
    public boolean isCustomRenderer() {
        return this.originalModel.isCustomRenderer();
    }

    @Override
    public TextureAtlasSprite getParticleIcon() {
        return this.originalModel.getParticleIcon();
    }

    @Override
    public ItemTransforms getTransforms() {
        return this.originalModel.getTransforms();
    }

    @Override
    public ItemOverrides getOverrides() {
        return this.originalModel.getOverrides();
    }
}
