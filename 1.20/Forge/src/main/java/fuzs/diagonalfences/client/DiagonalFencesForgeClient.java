package fuzs.diagonalfences.client;

import com.google.common.collect.Maps;
import fuzs.diagonalfences.DiagonalFences;
import fuzs.diagonalfences.api.world.level.block.DiagonalBlock;
import fuzs.diagonalfences.client.model.MultipartAppender;
import fuzs.puzzleslib.api.core.v1.ModConstructor;
import net.minecraft.client.renderer.block.BlockModelShaper;
import net.minecraft.client.renderer.block.model.multipart.MultiPart;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.FenceBlock;
import net.minecraft.world.level.block.IronBarsBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ModelEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLConstructModEvent;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;

@Mod.EventBusSubscriber(modid = DiagonalFences.MOD_ID, bus = Mod.EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
public class DiagonalFencesForgeClient {
    public static BiFunction<ResourceLocation, Material, TextureAtlasSprite> sprites;

    @SubscribeEvent
    public static void onConstructMod(final FMLConstructModEvent evt) {
        ModConstructor.construct(DiagonalFences.MOD_ID, DiagonalFences::new);
    }

    @SubscribeEvent
    public static void onModifyBakingResult(final ModelEvent.ModifyBakingResult evt) {
        Map<ResourceLocation, UnbakedModel> models = Maps.newHashMap();
        Map<Block, BakedModel> blockModels = Maps.newLinkedHashMap();
        for (Map.Entry<ResourceKey<Block>, Block> entry : BuiltInRegistries.BLOCK.entrySet()) {
            Block block = entry.getValue();
            if ((block instanceof FenceBlock || block instanceof IronBarsBlock) && block instanceof DiagonalBlock diagonalBlock && diagonalBlock.hasProperties()) {
                ModelResourceLocation modelLocation = BlockModelShaper.stateToModelLocation(block.getStateDefinition().any());
                UnbakedModel model = evt.getModelBakery().getModel(modelLocation);
                if (model instanceof MultiPart multiPart) {
                    MultipartAppender.appendDiagonalSelectors(models::put, multiPart, block instanceof IronBarsBlock);
                } else {
                    DiagonalFences.LOGGER.warn("Block '{}' is not using multipart models, diagonal connections will not be visible!", entry.getKey());
                }
                ModelBaker modelBaker = new ModelBaker() {

                    @Override
                    public UnbakedModel getModel(ResourceLocation modelLocation) {
                        if (models.containsKey(modelLocation)) return models.get(modelLocation);
                        return evt.getModelBakery().getModel(modelLocation);
                    }

                    @Nullable
                    @Override
                    public BakedModel bake(ResourceLocation modelLocation, ModelState p_251280_) {
                        return this.bake(modelLocation, p_251280_, this.getModelTextureGetter());
                    }

                    @Override
                    public @Nullable BakedModel bake(ResourceLocation location, ModelState state, Function<Material, TextureAtlasSprite> sprites) {
                        return this.getModel(location).bake(this, sprites, state, location);
                    }

                    @Override
                    public Function<Material, TextureAtlasSprite> getModelTextureGetter() {
                        return material -> {
                            Objects.requireNonNull(sprites, "sprites is null");
                            return sprites.apply(modelLocation, material);
                        };
                    }
                };
                blockModels.put(block, modelBaker.bake(modelLocation, BlockModelRotation.X0_Y0));
            }
        }
        for (Map.Entry<Block, BakedModel> entry : blockModels.entrySet()) {
            for (BlockState blockState : entry.getKey().getStateDefinition().getPossibleStates()) {
                evt.getModels().put(BlockModelShaper.stateToModelLocation(blockState), entry.getValue());
            }
        }
        sprites = null;
    }
}
