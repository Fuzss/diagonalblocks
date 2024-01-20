package fuzs.diagonalblocks.forge.client.core;

import fuzs.diagonalblocks.client.core.ClientAbstractions;
import fuzs.diagonalblocks.forge.client.resources.model.MultipartSegmentBakedModelForge;
import net.minecraft.client.renderer.ItemBlockRenderTypes;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.Block;

import java.util.List;
import java.util.Map;

public class ForgeClientAbstractions implements ClientAbstractions {

    @Override
    public BakedModel createWrappedBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {
        return new MultipartSegmentBakedModelForge(baseModel, quadMap);
    }

    @Override
    public void registerRenderType(Block block, RenderType renderType) {
        ItemBlockRenderTypes.setRenderLayer(block, renderType);
    }
}
