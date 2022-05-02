package fuzs.diagonalfences.client.model;

import fuzs.diagonalfences.block.IEightWayBlock;
import fuzs.diagonalfences.util.EightWayDirection;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.MultiPartBakedModel;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraftforge.client.model.data.EmptyModelData;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import org.apache.commons.lang3.tuple.Pair;

import java.lang.reflect.Field;
import java.util.*;
import java.util.function.Predicate;

public class MultipartAppender {

    private static final Field SELECTOR_FIELD = ObfuscationReflectionHelper.findField(MultiPartBakedModel.class, "f_119459_");
    private static final Random RAND = new Random();

    /**
     * Append the multipart model selectors needed for the diagonal arms of the fence, wall, etc.
     * @param block The Block to which the model belongs
     * @param oneArmStates All BlockStates that signify a single arm being present associated with the direction the arm points in
     * @param model The original multipart model
     * @return The new multipart model with the original selectors and the new diagonal selectors
     */
    public static MultiPartBakedModel appendDiagonalSelectors(Block block, Map<BlockState, Direction> oneArmStates, MultiPartBakedModel model) {

        List<Pair<Predicate<BlockState>, BakedModel>> selectors = new ArrayList<>(getMultiPartSelectors(model));

        List<Pair<Predicate<BlockState>, BakedModel>> newSelectors = new ArrayList<>();
        for (Pair<Predicate<BlockState>, BakedModel> selector : selectors) {

            for (Map.Entry<BlockState, Direction> armEntry : oneArmStates.entrySet()) {

                //Filter out the fence/wall post
                if (selector.getKey().test(block.defaultBlockState())) {

                    continue;
                }

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

        return new MultiPartBakedModel(selectors);
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

        return new MultipartSegmentBakedModel(segmentModel, quadMap);
    }

    /**
     * Rotate all {@link BakedQuad}s with the given {@link Direction cullFace} of the given {@link BakedModel segmentModel} 45 degrees clockwise.
     * The quads are duplicated, rotated and have their vertex normals recalculated.
     */
    private static void rotateQuads(Map<Direction, List<BakedQuad>> quadMap, BlockState state, BakedModel segmentModel, Direction cullFace, Direction segmentDir) {

        List<BakedQuad> quads = segmentModel.getQuads(state, cullFace, RAND, EmptyModelData.INSTANCE);
        List<BakedQuad> newQuads = new ArrayList<>();

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
        return IEightWayBlock.DIRECTION_TO_PROPERTY_MAP.get(dir);
    }

    private static List<Pair<Predicate<BlockState>, BakedModel>> getMultiPartSelectors(MultiPartBakedModel model) {

        try {

            //noinspection unchecked
            return (List<Pair<Predicate<BlockState>, BakedModel>>) SELECTOR_FIELD.get(model);
        }
        catch (IllegalAccessException e) {

            throw new RuntimeException(e);
        }
    }
}
