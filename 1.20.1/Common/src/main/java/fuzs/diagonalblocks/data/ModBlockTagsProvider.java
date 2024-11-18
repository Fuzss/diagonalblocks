package fuzs.diagonalblocks.data;

import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.diagonalblocks.init.ModRegistry;
import fuzs.puzzleslib.api.data.v2.AbstractTagProvider;
import fuzs.puzzleslib.api.data.v2.core.DataProviderContext;
import net.minecraft.core.HolderLookup;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public class ModBlockTagsProvider extends AbstractTagProvider.Blocks {
    private static final List<String> BUILT_IN_BLACKLISTED_FENCES = List.of("betternether:nether_reed_fence",
            "betterend:pythadendron_fence",
            "betternether:rubeus_fence",
            "betterend:dragon_tree_fence",
            "betterend:lucernia_fence",
            "betternether:mushroom_fir_fence",
            "betternether:stalagnate_fence",
            "betterend:tenanea_fence",
            "betternether:nether_sakura_fence",
            "betterend:umbrella_tree_fence",
            "betterend:mossy_glowshroom_fence",
            "betterend:helix_tree_fence",
            "betterend:lacugrove_fence",
            "betterend:jellyshroom_fence",
            "betternether:nether_mushroom_fence",
            "betternether:wart_fence",
            "betterend:end_lotus_fence",
            "betternether:anchor_tree_fence",
            "betternether:willow_fence",
            "domum_ornamentum:vanilla_fence_compat"
    );
    private static final List<String> TAG_BLACKLISTED_FENCES = List.of("immersiveengineering:treated_fence",
            "immersiveengineering:steel_fence"
    );
    private static final List<String> BUILT_IN_BLACKLISTED_WINDOWS = List.of("domum_ornamentum:blockpaperwall");
    private static final List<String> TAG_BLACKLISTED_WINDOWS = List.of("chipped:arched_lime_stained_glass_pane_pillar",
            "chipped:small_light_blue_stained_glass_pane",
            "chipped:arched_blue_stained_glass_pane_pillar",
            "chipped:oak_bordered_glass_pane",
            "chipped:raster_purple_stained_glass_pane",
            "chipped:tiled_lime_stained_glass_pane",
            "chipped:fancy_pink_stained_glass_pane",
            "chipped:ornate_red_stained_glass_pane",
            "chipped:fancy_light_gray_stained_glass_pane",
            "chipped:square_oak_glass_pane",
            "chipped:ornate_light_gray_stained_glass_pane",
            "chipped:ornate_lime_stained_glass_pane",
            "chipped:arched_cyan_stained_glass_pane_pillar",
            "chipped:fancy_cyan_stained_glass_pane",
            "chipped:ornate_yellow_stained_glass_pane",
            "chipped:ornate_white_stained_glass_pane",
            "chipped:tiled_cyan_stained_glass_pane",
            "chipped:tiled_light_blue_stained_glass_pane",
            "chipped:fancy_gray_stained_glass_pane",
            "chipped:raster_blue_stained_glass_pane",
            "chipped:small_red_stained_glass_pane",
            "chipped:raster_magenta_stained_glass_pane",
            "chipped:tiled_blue_stained_glass_pane",
            "chipped:tiled_brown_stained_glass_pane",
            "chipped:small_yellow_stained_glass_pane",
            "chipped:small_green_stained_glass_pane",
            "chipped:small_purple_stained_glass_pane",
            "chipped:oak_diamond_bordered_glass_pane",
            "chipped:tiled_green_stained_glass_pane",
            "chipped:small_light_gray_stained_glass_pane",
            "chipped:arched_red_stained_glass_pane_pillar",
            "chipped:tiled_gray_stained_glass_pane",
            "chipped:raster_red_stained_glass_pane",
            "chipped:clear_leaded_glass_pane",
            "chipped:raster_green_stained_glass_pane",
            "chipped:tiled_yellow_stained_glass_pane",
            "chipped:raster_pink_stained_glass_pane",
            "chipped:arched_purple_stained_glass_pane_pillar",
            "chipped:ornate_purple_stained_glass_pane",
            "chipped:small_diamond_leaded_glass_pane",
            "chipped:tiled_magenta_stained_glass_pane",
            "chipped:arched_leaded_glass_pane_pillar",
            "chipped:fancy_black_stained_glass_pane",
            "chipped:fancy_blue_stained_glass_pane",
            "chipped:small_brown_stained_glass_pane",
            "chipped:oak_horizontal_lined_glass_pane",
            "chipped:ornate_pink_stained_glass_pane",
            "chipped:ornate_brown_stained_glass_pane",
            "chipped:arched_light_blue_stained_glass_pane_pillar",
            "chipped:arched_brown_stained_glass_pane_pillar",
            "chipped:raster_brown_stained_glass_pane",
            "chipped:raster_white_stained_glass_pane",
            "chipped:ornate_cyan_stained_glass_pane",
            "chipped:raster_lime_stained_glass_pane",
            "chipped:fancy_magenta_stained_glass_pane",
            "chipped:small_blue_stained_glass_pane",
            "chipped:tiled_pink_stained_glass_pane",
            "chipped:fancy_red_stained_glass_pane",
            "chipped:small_magenta_stained_glass_pane",
            "chipped:arched_light_gray_stained_glass_pane_pillar",
            "chipped:raster_leaded_glass_pane",
            "chipped:fancy_light_blue_stained_glass_pane",
            "chipped:oak_woven_glass_pane",
            "chipped:fancy_yellow_stained_glass_pane",
            "chipped:raster_yellow_stained_glass_pane",
            "chipped:oak_line_bared_glass_pane",
            "chipped:ornate_light_blue_stained_glass_pane",
            "chipped:small_gray_stained_glass_pane",
            "chipped:arched_pink_stained_glass_pane_pillar",
            "chipped:oak_large_diamond_glass_pane",
            "chipped:small_orange_stained_glass_pane",
            "chipped:fancy_lime_stained_glass_pane",
            "chipped:small_lime_stained_glass_pane",
            "chipped:fancy_orange_stained_glass_pane",
            "chipped:arched_white_stained_glass_pane_pillar",
            "chipped:fancy_brown_stained_glass_pane",
            "chipped:arched_magenta_stained_glass_pane_pillar",
            "chipped:tiled_purple_stained_glass_pane",
            "chipped:ornate_gray_stained_glass_pane",
            "chipped:ornate_blue_stained_glass_pane",
            "chipped:oak_bared_glass_pane",
            "chipped:raster_light_gray_stained_glass_pane",
            "chipped:tiled_orange_stained_glass_pane",
            "chipped:ornate_magenta_stained_glass_pane",
            "chipped:small_pink_stained_glass_pane",
            "chipped:arched_orange_stained_glass_pane_pillar",
            "chipped:raster_light_blue_stained_glass_pane",
            "chipped:arched_black_stained_glass_pane_pillar",
            "chipped:arched_gray_stained_glass_pane_pillar",
            "chipped:small_cyan_stained_glass_pane",
            "chipped:ornate_leaded_glass_pane",
            "chipped:fancy_white_stained_glass_pane",
            "chipped:raster_gray_stained_glass_pane",
            "chipped:tiled_white_stained_glass_pane",
            "chipped:tiled_black_stained_glass_pane",
            "chipped:fancy_green_stained_glass_pane",
            "chipped:ornate_green_stained_glass_pane",
            "chipped:small_white_stained_glass_pane",
            "chipped:arched_yellow_stained_glass_pane_pillar",
            "chipped:raster_cyan_stained_glass_pane",
            "chipped:tiled_red_stained_glass_pane",
            "chipped:ornate_black_stained_glass_pane",
            "chipped:raster_black_stained_glass_pane",
            "chipped:ornate_orange_stained_glass_pane",
            "chipped:raster_orange_stained_glass_pane",
            "chipped:small_black_stained_glass_pane",
            "chipped:fancy_purple_stained_glass_pane",
            "chipped:oak_ornate_bared_glass_pane",
            "chipped:tiled_light_gray_stained_glass_pane",
            "chipped:arched_green_stained_glass_pane_pillar"
    );
    private static final List<String> BUILT_IN_BLACKLISTED_WALLS = List.of("betterend:end_lotus_wall",
            "betterend:sulphuric_rock_wall",
            "betterend:smaragdant_crystal_wall",
            "betterend:dragon_tree_wall",
            "betterend:umbrella_tree_wall",
            "betterend:umbralith_bricks_wall",
            "betterend:virid_jadestone_wall",
            "betternether:rubeus_wall",
            "betterend:lucernia_wall",
            "betterend:flavolite_bricks_wall",
            "betternether:anchor_tree_wall",
            "betternether:basalt_bricks_wall",
            "betterend:violecite_wall",
            "betterend:smaragdant_crystal_bricks_wall",
            "betternether:nether_brick_wall",
            "betternether:cincinnasite_wall",
            "betterend:end_stone_brick_weathered_wall",
            "betternether:willow_wall",
            "betternether:wart_wall",
            "betterend:mossy_glowshroom_wall",
            "betterend:pythadendron_wall",
            "betterend:virid_jadestone_bricks_wall",
            "betterend:flavolite_wall",
            "betterend:violecite_bricks_wall",
            "betterend:jellyshroom_wall",
            "betternether:nether_sakura_wall",
            "betterend:lacugrove_wall",
            "betternether:stalagnate_wall",
            "betterend:tenanea_wall",
            "betterend:helix_tree_wall",
            "betternether:bone_wall",
            "betterend:umbralith_wall",
            "betternether:nether_reed_wall",
            "betternether:nether_mushroom_wall",
            "betterend:azure_jadestone_bricks_wall",
            "betterend:end_stone_brick_cracked_wall",
            "betterend:sandy_jadestone_bricks_wall",
            "betterend:sandy_jadestone_wall",
            "betterend:azure_jadestone_wall",
            "betternether:mushroom_fir_wall",
            "betterend:sulphuric_rock_bricks_wall",
            "betternether:soul_sandstone_wall",
            "domum_ornamentum:vanilla_wall_compat"
    );
    private static final List<String> TAG_BLACKLISTED_WALLS = List.of();
    public static final Map<DiagonalBlockType, List<String>> BUILT_IN_BLACKLISTED_TYPES = Map.of(DiagonalBlockTypes.FENCE,
            BUILT_IN_BLACKLISTED_FENCES,
            DiagonalBlockTypes.WINDOW,
            BUILT_IN_BLACKLISTED_WINDOWS,
            DiagonalBlockTypes.WALL,
            BUILT_IN_BLACKLISTED_WALLS
    );
    public static final Map<DiagonalBlockType, List<String>> TAG_BLACKLISTED_TYPES = Map.of(DiagonalBlockTypes.FENCE,
            TAG_BLACKLISTED_FENCES,
            DiagonalBlockTypes.WINDOW,
            TAG_BLACKLISTED_WINDOWS,
            DiagonalBlockTypes.WALL,
            TAG_BLACKLISTED_WALLS
    );

    public ModBlockTagsProvider(DataProviderContext context) {
        super(context);
    }

    @Override
    public void addTags(HolderLookup.Provider provider) {
        this.tag(ModRegistry.NEVER_BLOCKS_DIAGONAL_CONNECTIONS_BLOCK_TAG).add(Blocks.SNOW);
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            IntrinsicTagAppender<Block> tagAppender = this.tag(type.getBlacklistTagKey());
            TAG_BLACKLISTED_TYPES.getOrDefault(type, Collections.emptyList())
                    .stream()
                    .map(ResourceLocation::new)
                    .forEach(tagAppender::addOptional);
        }
    }
}
