package com.fuzs.diagonalfences.element;

import com.fuzs.diagonalfences.DiagonalFences;
import com.fuzs.diagonalfences.client.element.DiagonalFencesExtension;
import com.fuzs.puzzleslib_df.element.extension.ClientExtensibleElement;
import com.fuzs.puzzleslib_df.json.JsonConfigFileUtil;
import com.google.common.collect.Lists;
import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.resources.VirtualAssetsPack;
import net.minecraft.resources.FilePack;
import net.minecraft.resources.IResourcePack;
import net.minecraft.resources.ResourcePack;
import net.minecraft.resources.ResourcePackType;
import net.minecraft.tags.BlockTags;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.event.InputEvent;
import net.minecraftforge.common.Tags;
import org.lwjgl.glfw.GLFW;
import org.lwjgl.system.CallbackI;

import java.io.*;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class DiagonalFencesElement extends ClientExtensibleElement<DiagonalFencesExtension> {

    public static final Tags.IOptionalNamedTag<Block> NON_DIAGONAL_FENCES_TAG = BlockTags.createOptional(new ResourceLocation(DiagonalFences.MODID, "non_diagonal_fences"));

    public DiagonalFencesElement() {

        super(element -> new DiagonalFencesExtension((DiagonalFencesElement) element));
    }

    @Override
    public String[] getDescription() {

        return new String[]{"Fences connecting diagonally? Wait. That's illegal."};
    }

    @Override
    public void setupCommon() {

        this.addListener(this::onKeyInput);
    }

    private void onKeyInput(final InputEvent.KeyInputEvent evt) {

        if (evt.getKey() == GLFW.GLFW_KEY_F12) {

            File file = new File("/Users/henning/Desktop/assets");
            Minecraft.getInstance().getResourceManager().getResourcePackStream().forEach(z -> {

                System.out.println(z.getName() + " " + z.getClass());
            });
            Minecraft.getInstance().getResourceManager().getResourcePackStream().filter(t -> t instanceof FilePack).forEach(x -> {

                System.out.println("working on " + x.getName() + " " + x.getClass().getName());
                this.on((FilePack) x, "");
            });
        }
    }

    private void on(FilePack pack, String outPath) {

        File file = new File(outPath);
        ZipFile zipfile;
        try {
            zipfile = new ZipFile(pack.file);
        } catch (IOException ioexception) {
            return;
        }

        Enumeration<? extends ZipEntry> enumeration = zipfile.entries();

        while(enumeration.hasMoreElements()) {
            ZipEntry zipentry = enumeration.nextElement();
            if (!zipentry.isDirectory()) {
                try {
                    InputStream initialStream = zipfile.getInputStream(zipentry);
                    File targetFile = new File(file, zipentry.getName());
                    JsonConfigFileUtil.mkdirs(targetFile.getParentFile());
                    byte[] buffer = new byte[initialStream.available()];
                    initialStream.read(buffer);
                    OutputStream outStream = new FileOutputStream(targetFile);
                    outStream.write(buffer);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

}
