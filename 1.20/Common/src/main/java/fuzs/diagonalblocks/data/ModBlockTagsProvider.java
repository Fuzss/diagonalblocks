package fuzs.diagonalblocks.data;

import fuzs.diagonalblocks.api.v2.DiagonalBlockType;
import fuzs.diagonalblocks.api.v2.DiagonalBlockTypes;
import fuzs.puzzleslib.api.data.v2.AbstractTagProvider;
import fuzs.puzzleslib.api.data.v2.core.DataProviderContext;
import net.minecraft.core.HolderLookup;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public class ModBlockTagsProvider extends AbstractTagProvider.Blocks {
    private static final List<String> BLACKLISTED_FENCES = List.of("betternether:nether_reed_fence", "betterend:pythadendron_fence", "betternether:rubeus_fence", "betterend:dragon_tree_fence", "betterend:lucernia_fence", "betternether:mushroom_fir_fence", "betternether:stalagnate_fence", "betterend:tenanea_fence", "betternether:nether_sakura_fence", "betterend:umbrella_tree_fence", "betterend:mossy_glowshroom_fence", "betterend:helix_tree_fence", "betterend:lacugrove_fence", "betterend:jellyshroom_fence", "betternether:nether_mushroom_fence", "betternether:wart_fence", "betterend:end_lotus_fence", "betternether:anchor_tree_fence", "betternether:willow_fence");
    private static final List<String> BLACKLISTED_WALLS = List.of("betterend:end_lotus_wall", "betterend:sulphuric_rock_wall", "betterend:smaragdant_crystal_wall", "betterend:dragon_tree_wall", "betterend:umbrella_tree_wall", "betterend:umbralith_bricks_wall", "betterend:virid_jadestone_wall", "betternether:rubeus_wall", "betterend:lucernia_wall", "betterend:flavolite_bricks_wall", "betternether:anchor_tree_wall", "betternether:basalt_bricks_wall", "betterend:violecite_wall", "betterend:smaragdant_crystal_bricks_wall", "betternether:nether_brick_wall", "betternether:cincinnasite_wall", "betterend:end_stone_brick_weathered_wall", "betternether:willow_wall", "betternether:wart_wall", "betterend:mossy_glowshroom_wall", "betterend:pythadendron_wall", "betterend:virid_jadestone_bricks_wall", "betterend:flavolite_wall", "betterend:violecite_bricks_wall", "betterend:jellyshroom_wall", "betternether:nether_sakura_wall", "betterend:lacugrove_wall", "betternether:stalagnate_wall", "betterend:tenanea_wall", "betterend:helix_tree_wall", "betternether:bone_wall", "betterend:umbralith_wall", "betternether:nether_reed_wall", "betternether:nether_mushroom_wall", "betterend:azure_jadestone_bricks_wall", "betterend:end_stone_brick_cracked_wall", "betterend:sandy_jadestone_bricks_wall", "betterend:sandy_jadestone_wall", "betterend:azure_jadestone_wall", "betternether:mushroom_fir_wall", "betterend:sulphuric_rock_bricks_wall", "betternether:soul_sandstone_wall");
    private static final Map<DiagonalBlockType, List<String>> BLACKLISTED_TYPES = Map.of(DiagonalBlockTypes.FENCE, BLACKLISTED_FENCES, DiagonalBlockTypes.WALL, BLACKLISTED_WALLS);

    public ModBlockTagsProvider(DataProviderContext context) {
        super(context);
    }

    @Override
    protected void addTags(HolderLookup.Provider provider) {
        for (DiagonalBlockType type : DiagonalBlockType.TYPES) {
            IntrinsicTagAppender<Block> tagAppender = this.tag(type.getBlacklistTagKey());
            BLACKLISTED_TYPES.getOrDefault(type, Collections.emptyList()).stream().map(ResourceLocation::new).forEach(tagAppender::addOptional);
        }

    }
}
