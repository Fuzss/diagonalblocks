package fuzs.diagonalblocks.neoforge.client.resources.model;

import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.model.BakedModelWrapper;
import net.neoforged.neoforge.client.model.data.ModelData;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;

public final class MultipartSegmentBakedModelForge extends BakedModelWrapper<BakedModel> {
    private final Map<Direction, List<BakedQuad>> quadMap;

    public MultipartSegmentBakedModelForge(BakedModel originalModel, Map<Direction, List<BakedQuad>> quadMap) {
        super(originalModel);
        this.quadMap = quadMap;
    }

    @NotNull
    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, @NotNull RandomSource rand, @NotNull ModelData extraData, @Nullable RenderType renderType) {
        return this.quadMap.get(side);
    }

    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, RandomSource rand) {
        return this.quadMap.get(side);
    }
}
