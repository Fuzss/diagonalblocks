package com.fuzs.puzzleslib_df.client.util;

import net.minecraft.client.AbstractOption;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.IGuiEventListener;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.gui.widget.Widget;
import net.minecraft.client.gui.widget.button.OptionButton;
import net.minecraft.client.gui.widget.list.OptionsRowList;

import java.util.ListIterator;
import java.util.Optional;

@SuppressWarnings("unused")
public class GameOptionsHelper {

    public static void addOptionToScreen(Screen screen, AbstractOption option) {

        Optional<OptionsRowList> optionalOptionsRowList = getOptionsRowList(screen);
        if (optionalOptionsRowList.isPresent()) {

            OptionsRowList optionsRowList = optionalOptionsRowList.get();
            if (!isOptionPresent(optionsRowList, option)) {

                addOptionToList(optionsRowList, option);
            }
        }
    }

    private static Optional<OptionsRowList> getOptionsRowList(Screen screen) {

        // don't access optionsRowList field directly using reflection or an accessor since OptiFine removes it
        return screen.getEventListeners().stream()
                .filter(listener -> listener instanceof OptionsRowList)
                .findFirst()
                .map(listener -> (OptionsRowList) listener);
    }

    private static boolean isOptionPresent(OptionsRowList optionsRowList, AbstractOption option) {

        return optionsRowList.getEventListeners().stream()
                .flatMap(row -> row.getEventListeners().stream())
                .filter(widget -> widget instanceof OptionButton)
                .map(widget -> (OptionButton) widget)
                // getEnumOptions
                .map(OptionButton::func_238517_a_)
                .anyMatch(currentOption -> currentOption == option);
    }

    private static void addOptionToList(OptionsRowList optionsRowList, AbstractOption option) {

        if (!tryAddOptionToRow(optionsRowList, option)) {

            optionsRowList.addOption(option, null);
        }
    }

    private static boolean tryAddOptionToRow(OptionsRowList optionsRowList, AbstractOption configOption) {

        ListIterator<OptionsRowList.Row> iterator = optionsRowList.getEventListeners().listIterator();
        while (iterator.hasNext()) {

            OptionsRowList.Row currentRow = iterator.next();
            if (currentRow.getEventListeners().size() == 1) {

                IGuiEventListener listener = currentRow.getEventListeners().get(0);
                // all buttons with two in one row will have this size
                if (((Widget) listener).getWidth() == 150 && listener instanceof OptionButton) {

                    AbstractOption option = ((OptionButton) listener).func_238517_a_();
                    OptionsRowList.Row newRow = OptionsRowList.Row.create(Minecraft.getInstance().gameSettings, optionsRowList.getWidth(), option, configOption);
                    iterator.set(newRow);

                    return true;
                }
            }
        }

        return false;
    }

}
