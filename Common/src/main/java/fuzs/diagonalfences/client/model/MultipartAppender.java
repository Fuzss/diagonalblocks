package fuzs.diagonalfences.client.model;

import com.google.common.collect.Lists;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.core.ClientAbstractions;
import fuzs.diagonalfences.core.EightWayDirection;
import fuzs.diagonalfences.mixin.client.accessor.KeyValueConditionAccessor;
import fuzs.diagonalfences.mixin.client.accessor.SelectorAccessor;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.core.Direction;
import net.minecraft.core.Registry;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockState;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class MultipartAppender {

    public static void onPrepareModelBaking(ModelBakery modelBakery) {
        Registry.BLOCK.stream()
                .filter(block -> block instanceof FenceBlock && block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties())
                .map(block -> block.getStateDefinition().any())
                .forEach(state -> {
                    if (modelBakery.getModel(BlockModelShaper.stateToModelLocation(state)) instanceof MultiPart multiPart) {
                        appendDiagonalSelectors(state, multiPart);
                    } else {
                        DiagonalFences.LOGGER.info("Fence block '{}' is not using multipart models, diagonal fence connections will not be visible!", state.getBlock());
                    }
                });
    }

    /**
     * Append the multipart model selectors needed for the diagonal arms of the fence, wall, etc.
     *
     * @param state     the block state to which the model belongs
     * @param multiPart the original multipart unbaked model
     */
    public static void appendDiagonalSelectors(BlockState state, MultiPart multiPart) {
        List<Selector> selectors = multiPart.getSelectors();
        List<Selector> newSelectors = Lists.newArrayList();

        for (Selector selector : selectors) {
            if (((SelectorAccessor) selector).diagonalfences$getCondition() instanceof KeyValueCondition condition) {
                if (Objects.equals(((KeyValueConditionAccessor) condition).diagonalfences$getValue(), "true")) {
                    EightWayDirection direction = EightWayDirection.byName(((KeyValueConditionAccessor) condition).diagonalfences$getKey());
                    if (direction != null) {
                        EightWayDirection interDirection = direction.rotateClockwise();
                        KeyValueCondition otherCondition = new KeyValueCondition(interDirection.getSerializedName(), "true");
                        MultiVariantAdapter variant = new MultiVariantAdapter(selector.getVariant().getVariants(), state, direction.toDirection());
                        newSelectors.add(new Selector(otherCondition, variant));
                    }
                }
            }
        }

        selectors.addAll(newSelectors);
    }

    /**
     * Duplicate and rotate all {@link BakedQuad quads} from the given {@link BakedModel segmentModel} to produce a new
     * model for a diagonal segment
     */
    public static BakedModel rotateMultipartSegment(BlockState state, BakedModel segmentModel, Direction armDir) {
        Map<Direction, List<BakedQuad>> quadMap = new HashMap<>();
        rotateQuads(quadMap, state, segmentModel, null, armDir);

        for (Direction cullFace : Direction.values()) {
            rotateQuads(quadMap, state, segmentModel, cullFace, armDir);
        }

        return ClientAbstractions.INSTANCE.createWrappedBakedModel(segmentModel, quadMap);
    }

    /**
     * Rotate all {@link BakedQuad}s with the given {@link Direction cullFace} of the given {@link BakedModel segmentModel} 45 degrees clockwise.
     * The quads are duplicated, rotated and have their vertex normals recalculated.
     */
    private static void rotateQuads(Map<Direction, List<BakedQuad>> quadMap, BlockState state, BakedModel segmentModel, Direction cullFace, Direction segmentDir) {
        List<BakedQuad> quads = segmentModel.getQuads(state, cullFace, RandomSource.create());
        List<BakedQuad> newQuads = Lists.newArrayList();

        for (BakedQuad quad : quads) {
            BakedQuad copy = QuadUtils.duplicateQuad(quad);
            QuadUtils.rotateQuad(copy, segmentDir);
            newQuads.add(copy);
        }

        quadMap.put(cullFace, newQuads);
    }
}
