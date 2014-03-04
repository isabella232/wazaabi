/***********************************************************************************************************************
 * Copyright (c) 2008 Olivier Moises, 2014 Pavel Erofeev
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises - initial API and implementation
 *   Pavel Erofeev - rendering engine for JavaFX
***********************************************************************************************************************/

package org.eclipse.wazaabi.demo.ecna2014.core.ui;

import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;


public class Utils {

    public static Binding createBinding(boolean toUI, String property) {
        Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
        StringParameter source = EDPHandlersFactory.eINSTANCE.createStringParameter();
        StringParameter target = EDPHandlersFactory.eINSTANCE.createStringParameter();
        source.setName("source");
        target.setName("target");
        if (toUI) {
            source.setValue(property);
            target.setValue("@text");
        } else {
            source.setValue("@text");
            target.setValue(property);
        }
        binding.getParameters().add(source);
        binding.getParameters().add(target);

        if (toUI) {
            PropertyChangedEvent bindingEvent = EDPEventsFactory.eINSTANCE.createPropertyChangedEvent();
            bindingEvent.setPath(property);
            binding.getEvents().add(bindingEvent);
        } else {
            Event bindingEvent = EDPEventsFactory.eINSTANCE.createEvent();
            bindingEvent.setId("core:ui:focus:out");
            binding.getEvents().add(bindingEvent);
        }
        return binding;
    }

    public static PushButton createButton(String label, String action) {
        return createButton(label, action, "org.eclipse.wazaabi.demo.ecna2014.core.handlers.");
    }

    public static PushButton createButton(String label, String action, String prefix) {
        PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
        pushButton.setText(label);

        EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
        eventHandler.setUri(prefix + action);
        pushButton.getHandlers().add(eventHandler);

        Event event = EDPEventsFactory.eINSTANCE.createEvent();
        event.setId("core:ui:selection");
        eventHandler.getEvents().add(event);

        return pushButton;
    }

    public static Label createLabel(String text) {
        Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
        label.setText(text);
        return label;
    }

    public static TextComponent createText(boolean toUI, String property) {
        TextComponent text = CoreWidgetsFactory.eINSTANCE.createTextComponent();
        text.getHandlers().add(Utils.createBinding(toUI, property));

        BooleanRule br = CoreStylesFactory.eINSTANCE.createBooleanRule();
        br.setPropertyName("border");
        br.setValue(true);
        text.getStyleRules().add(br);

        return text;
    }
}
