package fuzs.diagonalfences.client.resources.model;

import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.core.Direction;
import org.joml.Matrix4f;
import org.joml.Quaternionf;
import org.joml.Vector3f;
import org.joml.Vector4f;

public class QuadUtils {
    private static final float ROTATION_ANGLE = -45F * 0.017453292F;
    //Scale factor at a 45 degree rotation
    private static final float SCALE_ROTATION_45 = 1.0F / (float) Math.cos(Math.PI / 4D) - 1.0F;
    private static final Vector3f ROTATION_ORIGIN = new Vector3f(.5F, .5F, .5F);
    private static final Matrix4f ROTATION_MATRIX = new Matrix4f().rotation(new Quaternionf().setAngleAxis(ROTATION_ANGLE, 0.0F, 1.0F, 0.0F));

    /**
     * Create a deep-copy of the given {@link BakedQuad quad}
     */
    public static BakedQuad duplicateQuad(BakedQuad quad) {

        int[] vertexData = new int[quad.getVertices().length];
        System.arraycopy(quad.getVertices(), 0, vertexData, 0, vertexData.length);

        return new BakedQuad(vertexData, quad.getTintIndex(), quad.getDirection(), quad.getSprite(), quad.isShade());
    }

    /**
     * Rotate the given {@link BakedQuad quad} 45 degree clockwise and recalculate its vertex normals
     *
     * @param quad The given BakedQuad, must be a deep-copy of the original
     * @param dir  The {@link Direction dir} in which the fence/wall arm this quad belongs to points
     */
    public static void rotateQuad(BakedQuad quad, Direction dir) {

        Vector3f scaleMult = new Vector3f(Math.abs(dir.getStepX()), 1, Math.abs(dir.getStepZ()));

        Vector3f scaleVec = new Vector3f(1.0F, 0.0F, 1.0F);
        scaleVec.mul(SCALE_ROTATION_45);
        scaleVec.mul(scaleMult.x(), scaleMult.y(), scaleMult.z());
        scaleVec.add(1.0F, 1.0F, 1.0F);

        int[] vertexData = quad.getVertices();
        float[][] pos = unpackQuadPosition(vertexData);

        for (int i = 0; i < 4; i++) {

            Vector4f vector4f = new Vector4f(pos[i][0] - ROTATION_ORIGIN.x(), pos[i][1] - ROTATION_ORIGIN.y(), pos[i][2] - ROTATION_ORIGIN.z(), 1.0F);
            vector4f.mul(new Vector4f(scaleVec, 1.0F));
            ROTATION_MATRIX.transform(vector4f);

            pos[i][0] = vector4f.x() + ROTATION_ORIGIN.x();
            pos[i][1] = vector4f.y() + ROTATION_ORIGIN.y();
            pos[i][2] = vector4f.z() + ROTATION_ORIGIN.z();
        }

        packQuadPositions(vertexData, pos);
        fillNormal(vertexData, pos);
    }

    /**
     * Unpack the vertex positions from the given vertex data
     */
    public static float[][] unpackQuadPosition(int[] vertexData) {

        float[][] pos = new float[4][3];

        int step = vertexData.length / 4; //This is needed to support the extended vertex formats used by shaders in OptiFine
        for (int i = 0; i < 4; i++) {

            int offset = i * step;
            pos[i][0] = Float.intBitsToFloat(vertexData[offset]);
            pos[i][1] = Float.intBitsToFloat(vertexData[offset + 1]);
            pos[i][2] = Float.intBitsToFloat(vertexData[offset + 2]);
        }

        return pos;
    }

    /**
     * Update the given vertex data with the given vertex positions
     */
    public static void packQuadPositions(int[] vertexData, float[][] pos) {

        int step = vertexData.length / 4; //This is needed to support the extended vertex formats used by shaders in OptiFine
        for (int i = 0; i < 4; i++) {

            int offset = i * step;
            vertexData[offset] = Float.floatToIntBits(pos[i][0]);
            vertexData[offset + 1] = Float.floatToIntBits(pos[i][1]);
            vertexData[offset + 2] = Float.floatToIntBits(pos[i][2]);
        }
    }

    /**
     * Calculate face normals from vertex positions and write them to the given vertex data
     * Adapted from net.minecraftforge.client.ForgeHooksClient#fillNormal(int[], Direction)
     */
    public static void fillNormal(int[] vertexData, float[][] pos) {

        Vector3f v1 = new Vector3f(pos[3][0], pos[3][1], pos[3][2]);
        Vector3f t1 = new Vector3f(pos[1][0], pos[1][1], pos[1][2]);
        Vector3f v2 = new Vector3f(pos[2][0], pos[2][1], pos[2][2]);
        Vector3f t2 = new Vector3f(pos[0][0], pos[0][1], pos[0][2]);

        v1.sub(t1);
        v2.sub(t2);
        v2.cross(v1);
        v2.normalize();

        int x = ((byte) Math.round(v2.x() * 127)) & 0xFF;
        int y = ((byte) Math.round(v2.y() * 127)) & 0xFF;
        int z = ((byte) Math.round(v2.z() * 127)) & 0xFF;

        int normal = x | (y << 0x08) | (z << 0x10);

        int step = vertexData.length / 4; //This is needed to support the extended vertex formats used by shaders in OptiFine
        for (int vert = 0; vert < 4; vert++) {
            vertexData[vert * step + 7] = normal;
        }
    }
}
