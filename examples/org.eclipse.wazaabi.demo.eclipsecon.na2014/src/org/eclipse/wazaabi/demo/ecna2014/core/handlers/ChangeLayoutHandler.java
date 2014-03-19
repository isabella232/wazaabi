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

import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ChangeLayoutHandler {

    private static final Logger log = LoggerFactory.getLogger(ChangeLayoutHandler.class);

    public ChangeLayoutHandler() {
        log.info("creating {}", getClass().getName());
    }

    public void execute(Widget dispatcher, EventHandler eventHandler, Event event) {
        if (dispatcher instanceof PushButton) {
            Container container = (Container) dispatcher.eContainer();
            StyleRule sr = container.getFirstStyleRule("layout", null);

            if (sr instanceof BoxLayoutRule) {
                container.getStyleRules().remove(sr);

                BoxLayoutRule blr = CoreStylesFactory.eINSTANCE.createBoxLayoutRule();
                blr.setPropertyName("layout");
                blr.setMargin(15);
                blr.setSpacing(5);

                if (((BoxLayoutRule) sr).getOrientation() == Orientation.HORIZONTAL)
                    blr.setOrientation(Orientation.VERTICAL);
                else
                    blr.setOrientation(Orientation.HORIZONTAL);
                container.getStyleRules().add(0, blr);
            }
        }
        log.info("widget clicked: {}", dispatcher.toString());
    }

    public void dispose() {
        log.info("disposing {}", getClass().getName());
    }
}
