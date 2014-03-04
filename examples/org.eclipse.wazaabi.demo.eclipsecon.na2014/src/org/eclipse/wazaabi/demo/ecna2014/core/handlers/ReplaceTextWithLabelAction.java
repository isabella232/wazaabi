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

package org.eclipse.wazaabi.demo.ecna2014.core.handlers;

import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ReplaceTextWithLabelAction {

    private static final Logger log = LoggerFactory.getLogger(ReplaceTextWithLabelAction.class);

    public ReplaceTextWithLabelAction() {
        log.info("creating {}", getClass().getName());
    }

    public void execute(Widget dispatcher, EventHandler eventHandler, Event event) {
        if (dispatcher instanceof PushButton) {
            Container container = (Container) dispatcher.eContainer();
            String text1 = ((TextComponent) container.getChildren().get(2)).getText();
            String text2 = ((TextComponent) container.getChildren().get(4)).getText();

            Label label1 = CoreWidgetsFactory.eINSTANCE.createLabel();
            label1.setText(text1);
            container.getChildren().remove(2);
            container.getChildren().add(2, label1);
            
            Label label2 = CoreWidgetsFactory.eINSTANCE.createLabel();
            label2.setText(text2);
            container.getChildren().remove(4);
            container.getChildren().add(4, label2);
            
            //log.debug("Replaced textcompo with label, text = {}", text1);
        }
        log.info("widget clicked: {}", dispatcher.toString());
    }

    public void dispose() {
        log.info("disposing {}", getClass().getName());
    }
}
