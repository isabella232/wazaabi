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

package org.eclipse.wazaabi.demo.ecna2014.fx.handlers;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.fx.styles.FXStylesFactory;
import org.eclipse.wazaabi.mm.fx.styles.HBoxRule;
import org.eclipse.wazaabi.mm.fx.styles.VBoxRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ChangeLayoutAction {

    private static final Logger log = LoggerFactory.getLogger(ChangeLayoutAction.class);

    public ChangeLayoutAction() {
        log.info("creating {}", getClass().getName());
    }

    public void execute(Widget dispatcher, EventHandler eventHandler, Event event) {
        if (dispatcher instanceof PushButton) {
            Container container = (Container) dispatcher.eContainer();
            StyleRule sr = container.getFirstStyleRule("layout", null);

            if (sr instanceof HBoxRule) {
                container.getStyleRules().remove(sr);
                VBoxRule layout = FXStylesFactory.eINSTANCE.createVBoxRule();
                layout.setSpacing(5);
                layout.setPropertyName("layout");
                container.getStyleRules().add(layout);
            } else if (sr instanceof VBoxRule) {
                container.getStyleRules().remove(sr);
                HBoxRule layout = FXStylesFactory.eINSTANCE.createHBoxRule();
                layout.setSpacing(10);
                layout.setPropertyName("layout");
                container.getStyleRules().add(layout);
            }
        }
        log.info("widget clicked: {}", dispatcher.toString());
    }

    public void dispose() {
        log.info("disposing {}", getClass().getName());
    }
}
