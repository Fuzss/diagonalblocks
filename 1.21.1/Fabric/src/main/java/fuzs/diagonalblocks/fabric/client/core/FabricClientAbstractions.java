package fuzs.diagonalblocks.fabric.client.core;

import fuzs.diagonalblocks.client.core.ClientAbstractions;
import fuzs.diagonalblocks.fabric.client.resources.model.MultipartSegmentBakedModelFabric;
import net.fabricmc.fabric.api.blockrenderlayer.v1.BlockRenderLayerMap;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.Block;

import java.util.List;
import java.util.Map;

public class FabricClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelFabric<>(baseModel, quadMap);
    }

    @Override
    public void registerRenderType(Block block, RenderType renderType) {
        BlockRenderLayerMap.INSTANCE.putBlock(block, renderType);
    }
}
