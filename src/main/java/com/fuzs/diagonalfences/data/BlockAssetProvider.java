package com.fuzs.diagonalfences.data;

import com.fuzs.diagonalfences.block.IEightWayBlock;
import com.fuzs.diagonalfences.client.renderer.model.RuntimeModelBuilder;
import com.google.gson.JsonElement;
import net.minecraft.block.Block;
import net.minecraft.data.*;
import net.minecraft.util.Direction;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.model.generators.ModelFile;

public class BlockAssetProvider {

    public static IFinishedBlockState getDiagonalState(Block block, ResourceLocation[] diagonalModels) {

        return FinishedMultiPartBlockState.func_240106_a_(block)
                .func_240108_a_(IMultiPartPredicateBuilder.func_240089_a_().func_240098_a_(IEightWayBlock.NORTH_EAST, true), BlockModelDefinition.getNewModelDefinition().replaceInfoValue(BlockModelFields.field_240202_c_, diagonalModels[0]).replaceInfoValue(BlockModelFields.field_240203_d_, true))
                .func_240108_a_(IMultiPartPredicateBuilder.func_240089_a_().func_240098_a_(IEightWayBlock.SOUTH_EAST, true), BlockModelDefinition.getNewModelDefinition().replaceInfoValue(BlockModelFields.field_240202_c_, diagonalModels[1]).replaceInfoValue(BlockModelFields.field_240201_b_, BlockModelFields.Rotation.R90).replaceInfoValue(BlockModelFields.field_240203_d_, true))
                .func_240108_a_(IMultiPartPredicateBuilder.func_240089_a_().func_240098_a_(IEightWayBlock.SOUTH_WEST, true), BlockModelDefinition.getNewModelDefinition().replaceInfoValue(BlockModelFields.field_240202_c_, diagonalModels[2]).replaceInfoValue(BlockModelFields.field_240201_b_, BlockModelFields.Rotation.R180).replaceInfoValue(BlockModelFields.field_240203_d_, true))
                .func_240108_a_(IMultiPartPredicateBuilder.func_240089_a_().func_240098_a_(IEightWayBlock.NORTH_WEST, true), BlockModelDefinition.getNewModelDefinition().replaceInfoValue(BlockModelFields.field_240202_c_, diagonalModels[3]).replaceInfoValue(BlockModelFields.field_240201_b_, BlockModelFields.Rotation.R270).replaceInfoValue(BlockModelFields.field_240203_d_, true));
    }

    public static JsonElement getVariantModel(ResourceLocation parent, ResourceLocation texture) {

        return new RuntimeModelBuilder()
                .parent(new ModelFile.UncheckedModelFile(parent))
                .texture("texture", texture)
                .toJson();
    }

    public static JsonElement getDiagonalModel() {

        return new RuntimeModelBuilder()
                .texture("particle", "#texture")
                .element()
                .from(15, 12, 0)
                .to(17, 15, 10)
                .rotation()
                .origin(16, 8, 0)
                .axis(Direction.Axis.Y)
                .angle(-45)
                .end()
                .face(Direction.DOWN)
                .uvs(7, 0, 9, 10)
                .texture("#texture")
                .end()
                .face(Direction.UP)
                .uvs(7, 0, 9, 10)
                .texture("#texture")
                .end()
                .face(Direction.NORTH)
                .uvs(7, 1, 9, 4)
                .texture("#texture")
                .end()
                .face(Direction.WEST)
                .uvs(0, 1, 10, 4)
                .texture("#texture")
                .end()
                .face(Direction.EAST)
                .uvs(0, 1, 10, 4)
                .texture("#texture")
                .end()
                .end()
                .element()
                .from(15, 6, 0)
                .to(17, 9, 10)
                .rotation()
                .origin(16, 8, 0)
                .axis(Direction.Axis.Y)
                .angle(-45)
                .end()
                .face(Direction.DOWN)
                .uvs(7, 0, 9, 10)
                .texture("#texture")
                .end()
                .face(Direction.UP)
                .uvs(7, 0, 9, 10)
                .texture("#texture")
                .end()
                .face(Direction.NORTH)
                .uvs(7, 7, 9, 10)
                .texture("#texture")
                .end()
                .face(Direction.WEST)
                .uvs(0, 7, 10, 10)
                .texture("#texture")
                .end()
                .face(Direction.EAST)
                .uvs(0, 7, 10, 10)
                .texture("#texture")
                .end()
                .end()
                .toJson();
    }

}
