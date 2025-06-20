package fuzs.diagonalblocks.client.resources.model;

import fuzs.puzzleslib.api.client.renderer.v1.model.QuadUtils;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.Util;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.BlockModelPart;
import net.minecraft.client.renderer.block.model.BlockStateModel;
import net.minecraft.client.renderer.block.model.SimpleModelWrapper;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.ModelBaker;
import net.minecraft.client.resources.model.QuadCollection;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import org.joml.Matrix4f;
import org.joml.Quaternionf;
import org.joml.Vector3f;
import org.joml.Vector4f;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;

public record RotatedVariant(BlockStateModel.Unbaked variant, Direction direction) implements BlockStateModel.Unbaked {
    static final float ROTATION_ANGLE = -45.0F * 0.017453292F;
    /**
     * Scale factor at a 45 degree rotation.
     */
    static final float SCALE_ROTATION_45 = 1.0F / (float) Math.cos(Math.PI / 4.0) - 1.0F;
    static final Vector3f ROTATION_ORIGIN = new Vector3f(0.5F, 0.5F, 0.5F);
    static final Matrix4f ROTATION_MATRIX = new Matrix4f().rotation(new Quaternionf().setAngleAxis(ROTATION_ANGLE,
            0.0F,
            1.0F,
            0.0F));
    static final Collection<Direction> VALID_QUAD_FACES = Util.make(new ArrayList<>(Arrays.asList(Direction.values())),
            (ArrayList<Direction> list) -> {
                list.add(null);
            });

    @Override
    public void resolveDependencies(Resolver resolver) {
        this.variant.resolveDependencies(resolver);
    }

    @Override
    public BlockStateModel bake(ModelBaker modelBaker) {
        Function<BlockModelPart, BlockModelPart> blockModelPartRotator = Util.memoize((BlockModelPart blockModelPart) -> {
            return rotateBlockModelPart(blockModelPart, this.direction);
        });
        BlockStateModel blockStateModel = this.variant.bake(modelBaker);
        return new BlockStateModel() {
            @Override
            public void collectParts(RandomSource randomSource, List<BlockModelPart> list) {
                List<BlockModelPart> tmpList = new ObjectArrayList<>();
                blockStateModel.collectParts(randomSource, tmpList);
                for (BlockModelPart blockModelPart : tmpList) {
                    list.add(blockModelPartRotator.apply(blockModelPart));
                }
            }

            @Override
            public TextureAtlasSprite particleIcon() {
                return blockStateModel.particleIcon();
            }
        };
    }

    /**
     * Duplicate and rotate all contained {@link BakedQuad BakedQuads} 45 degrees clockwise from the given
     * {@link BlockModelPart} to produce a new variant for a diagonal side.
     */
    static BlockModelPart rotateBlockModelPart(BlockModelPart blockModelPart, Direction direction) {

        QuadCollection.Builder builder = new QuadCollection.Builder();
        for (Direction face : VALID_QUAD_FACES) {
            for (BakedQuad bakedQuad : blockModelPart.getQuads(face)) {

                BakedQuad copiedBakedQuad = QuadUtils.copy(bakedQuad);
                rotateQuad(copiedBakedQuad, direction);
                if (face == null) {
                    builder.addUnculledFace(copiedBakedQuad);
                } else {
                    builder.addCulledFace(face, copiedBakedQuad);
                }
            }
        }

        return new SimpleModelWrapper(builder.build(),
                blockModelPart.useAmbientOcclusion(),
                blockModelPart.particleIcon());
    }

    /**
     * Rotate the given {@link BakedQuad} 45 degrees clockwise and recalculate its vertex normals.
     *
     * @author <a href="https://github.com/XFactHD">XFactHD</a>
     */
    static void rotateQuad(BakedQuad bakedQuad, Direction direction) {

        Vector3f scaleMultiplier = new Vector3f(Math.abs(direction.getStepX()), 1.0F, Math.abs(direction.getStepZ()));
        Vector3f scaleVector = new Vector3f(1.0F, 0.0F, 1.0F);
        scaleVector.mul(SCALE_ROTATION_45);
        scaleVector.mul(scaleMultiplier.x(), scaleMultiplier.y(), scaleMultiplier.z());
        scaleVector.add(1.0F, 1.0F, 1.0F);

        for (int i = 0; i < 4; i++) {
            Vector3f positionVector = QuadUtils.getPosition(bakedQuad, i);
            positionVector.sub(ROTATION_ORIGIN).mul(scaleVector);
            Vector4f transformVector = new Vector4f(positionVector, 1.0F);
            ROTATION_MATRIX.transform(transformVector);
            positionVector.set(transformVector).add(ROTATION_ORIGIN);
            QuadUtils.setPosition(bakedQuad, i, positionVector);
        }

        QuadUtils.fillNormal(bakedQuad);
    }
}
