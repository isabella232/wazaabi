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

package org.eclipse.wazaabi.demo.ecna2014.swt.handlers;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;
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

            if (sr instanceof RowLayoutRule) {
                container.getStyleRules().remove(sr);
                GridLayoutRule layout = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
                layout.setNumColumns(1);
                layout.setPropertyName("layout");
                container.getStyleRules().add(layout);
            } else if (sr instanceof GridLayoutRule) {
                container.getStyleRules().remove(sr);
                RowLayoutRule layout = SWTStylesFactory.eINSTANCE.createRowLayoutRule();
                layout.setPropertyName("layout");
                container.getStyleRules().add(layout);
            }
        }
    }

    public void dispose() {
        log.info("disposing {}", getClass().getName());
    }
}
