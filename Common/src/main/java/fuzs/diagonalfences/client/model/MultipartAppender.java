package fuzs.diagonalfences.client.model;

import com.google.common.collect.Lists;
import fuzs.diagonalfences.world.level.block.EightWayBlock;
import fuzs.diagonalfences.client.core.ClientModServices;
import fuzs.diagonalfences.mixin.client.accessor.MultiPartBakedModelAccessor;
import fuzs.diagonalfences.core.EightWayDirection;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import org.apache.commons.lang3.tuple.Pair;

import java.util.*;
import java.util.function.Predicate;

public class MultipartAppender {

    /**
     * Append the multipart model selectors needed for the diagonal arms of the fence, wall, etc.
     * @param block The Block to which the model belongs
     * @param oneArmStates All BlockStates that signify a single arm being present associated with the direction the arm points in
     * @param model The original multipart model
     * @return The new multipart model with the original selectors and the new diagonal selectors
     */
    public static Optional<BakedModel> appendDiagonalSelectors(Block block, Map<BlockState, Direction> oneArmStates, MultiPartBakedModel model, List<BlockState> testStates) {

        List<Pair<Predicate<BlockState>, BakedModel>> selectors = Lists.newArrayList(((MultiPartBakedModelAccessor) model).getSelectors());
        List<Pair<Predicate<BlockState>, BakedModel>> newSelectors = Lists.newArrayList();

        for (Pair<Predicate<BlockState>, BakedModel> selector : selectors) {

            // filter out the fence/wall post
            if (selector.getKey().test(block.defaultBlockState())) continue;

            // aiming for some sort of resource pack compatibility here, so avoid doing anything if at least one of our states is somehow present already
            for (BlockState testState : testStates) {
                if (selector.getKey().test(testState)) return Optional.empty();
            }

            for (Map.Entry<BlockState, Direction> armEntry : oneArmStates.entrySet()) {

                if (selector.getKey().test(armEntry.getKey())) {
                    BooleanProperty diagonalProp = getClockwiseIntercardinalProperty(armEntry.getValue());
                    newSelectors.add(Pair.of(
                            state -> state.getValue(diagonalProp),
                            rotateMultipartSegment(armEntry.getKey(), selector.getValue(), armEntry.getValue())
                    ));
                }
            }
        }

        selectors.addAll(newSelectors);
        return Optional.of(new MultiPartBakedModel(selectors));
    }

    /**
     * Duplicate and rotate all {@link BakedQuad quads} from the given {@link BakedModel segmentModel} to produce a new
     * model for a diagonal segment
     */
    private static BakedModel rotateMultipartSegment(BlockState state, BakedModel segmentModel, Direction armDir) {
        Map<Direction, List<BakedQuad>> quadMap = new HashMap<>();
        rotateQuads(quadMap, state, segmentModel, null, armDir);

        for (Direction cullFace : Direction.values()) {
            rotateQuads(quadMap, state, segmentModel, cullFace, armDir);
        }

        return ClientModServices.ABSTRACTIONS.createWrappedBakedModel(segmentModel, quadMap);
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

    /**
     * Get property for the clockwise intercardinal neighbor to the given cardinal direction
     */
    private static BooleanProperty getClockwiseIntercardinalProperty(Direction cardinal) {
        EightWayDirection dir = EightWayDirection.byIndex(cardinal.get2DDataValue(), true);
        return EightWayBlock.DIRECTION_TO_PROPERTY_MAP.get(dir);
    }
}
