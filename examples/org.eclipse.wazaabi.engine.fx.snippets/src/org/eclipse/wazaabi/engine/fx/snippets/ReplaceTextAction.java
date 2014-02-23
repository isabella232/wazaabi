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

package org.eclipse.wazaabi.engine.fx.snippets;

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


public class ReplaceTextAction {

    private static final Logger log = LoggerFactory.getLogger(ReplaceTextAction.class);

    public ReplaceTextAction() {
        log.info("creating {}", getClass().getName());
    }

    public void execute(Widget dispatcher, EventHandler eventHandler, Event event) {
        if (dispatcher instanceof PushButton) {
            Container container = (Container) dispatcher.eContainer();
            String text = ((TextComponent) container.getChildren().get(3)).getText();

            Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
            //PushButton label = CoreWidgetsFactory.eINSTANCE.createPushButton();
            label.setText(text);

            container.getChildren().remove(3);
            container.getChildren().add(3, label);
            //container.getChildren().add(label);
            log.debug("Replaced textcompo with label, text = {}", text);
        }
        log.info("widget clicked: {}", dispatcher.toString());
    }

    public void dispose() {
        log.info("disposing {}", getClass().getName());
    }
}
