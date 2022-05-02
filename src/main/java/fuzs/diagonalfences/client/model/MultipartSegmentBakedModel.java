package fuzs.diagonalfences.client.model;

import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.model.BakedModelWrapper;
import net.minecraftforge.client.model.data.IModelData;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class MultipartSegmentBakedModel extends BakedModelWrapper<BakedModel> {

    private final Map<Direction, List<BakedQuad>> quadMap;

    public MultipartSegmentBakedModel(BakedModel baseModel, Map<Direction, List<BakedQuad>> quadMap) {

        super(baseModel);
        this.quadMap = quadMap;
    }

    @NotNull
    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, @NotNull Random rand, @NotNull IModelData extraData) {

        return quadMap.get(side);
    }

    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, Random rand) {

        return quadMap.get(side);
    }
}
