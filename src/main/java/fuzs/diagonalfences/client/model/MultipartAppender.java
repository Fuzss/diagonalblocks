package fuzs.diagonalfences.client.model;

import com.google.common.base.Stopwatch;
import com.google.common.collect.Lists;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.core.EightWayDirection;
import fuzs.diagonalfences.mixin.client.accessor.KeyValueConditionAccessor;
import fuzs.diagonalfences.mixin.client.accessor.ModelBakeryAccessor;
import fuzs.diagonalfences.mixin.client.accessor.SelectorAccessor;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.MultiVariant;
import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.renderer.block.model.multipart.KeyValueCondition;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.block.model.multipart.Selector;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelBakery;
import net.minecraft.client.resources.model.ModelResourceLocation;
import net.minecraft.core.Direction;
import net.minecraft.core.Registry;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.model.data.EmptyModelData;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class MultipartAppender {
    private static final Random RANDOM = new Random();

    public static void onPrepareModelBaking(ModelBakery modelBakery) {
        Stopwatch stopwatch = Stopwatch.createStarted();
        Registry.BLOCK.stream()
                .filter(block -> block instanceof FenceBlock && block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties())
                .map(block -> block.getStateDefinition().any())
                .forEach(state -> {

                    if (modelBakery.getModel(BlockModelShaper.stateToModelLocation(state)) instanceof MultiPart multiPart) {
                        appendDiagonalSelectors(modelBakery, multiPart);
                    } else {
                        DiagonalFences.LOGGER.warn("Fence block '{}' is not using multipart models, diagonal fence connections will not be visible!", state.getBlock());
                    }
                });

        DiagonalFences.LOGGER.info("Constructing diagonal fence block models took {}ms", stopwatch.stop().elapsed().toMillis());
    }

    /**
     * Append the multipart variant selectors needed for the diagonal arms of the fence, wall, etc.
     *
     * @param multiPart the original multipart unbaked variant
     */
    public static void appendDiagonalSelectors(ModelBakery modelBakery, MultiPart multiPart) {
        List<Selector> selectors = multiPart.getSelectors();
        List<Selector> newSelectors = Lists.newArrayList();

        for (Selector selector : selectors) {

            if (((SelectorAccessor) selector).diagonalfences$getCondition() instanceof KeyValueCondition condition) {

                if (Objects.equals(((KeyValueConditionAccessor) condition).diagonalfences$getValue(), "true")) {

                    EightWayDirection direction = EightWayDirection.byName(((KeyValueConditionAccessor) condition).diagonalfences$getKey());
                    if (direction != null) {

                        EightWayDirection interDirection = direction.rotateClockwise();
                        KeyValueCondition newCondition = new KeyValueCondition(interDirection.getSerializedName(), "true");
                        List<Variant> variants = selector.getVariant().getVariants();
                        List<Variant> newVariants = Lists.newArrayList();
                        for (Variant variant : variants) {

                            ModelResourceLocation location = new ModelResourceLocation(variant.getModelLocation(), interDirection.getSerializedName());
                            // this is the rotated model part, just make sure it is cached somewhere to avoid recreation multiple times since it's very expensive
                            // we could also use our own cache, but unbaked models cache works fine
                            ((ModelBakeryAccessor) modelBakery).diagonalfences$callCacheAndQueueDependencies(location, new RotatedVariant(variant, direction.toDirection()));
                            // copy old variant which is now backed by the rotated model part, besides that only the weight value should really matter
                            newVariants.add(new Variant(location, variant.getRotation(), variant.isUvLocked(), variant.getWeight()));
                        }

                        newSelectors.add(new Selector(newCondition, new MultiVariant(newVariants)));
                    }
                }
            }
        }

        selectors.addAll(newSelectors);
    }

    /**
     * Duplicate and rotate all {@link BakedQuad quads} from the given {@link BakedModel segmentModel} to produce a new
     * variant for a diagonal segment
     */
    public static BakedModel rotateMultipartSegment(@Nullable BlockState state, BakedModel segmentModel, Direction armDir) {
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
    private static void rotateQuads(Map<Direction, List<BakedQuad>> quadMap, @Nullable BlockState state, BakedModel segmentModel, Direction cullFace, Direction segmentDir) {
        List<BakedQuad> quads = segmentModel.getQuads(state, cullFace, RANDOM, EmptyModelData.INSTANCE);
        List<BakedQuad> newQuads = Lists.newArrayList();

        for (BakedQuad quad : quads) {
            BakedQuad copy = QuadUtils.duplicateQuad(quad);
            QuadUtils.rotateQuad(copy, segmentDir);
            newQuads.add(copy);
        }

        quadMap.put(cullFace, newQuads);
    }
}
