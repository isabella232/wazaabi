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

package org.eclipse.wazaabi.engine.gwt.nonosgi;

import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.nonosgi.CoreHelper;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.events.EventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.engine.gwt.editparts.GWTEditPartFactory;
import org.eclipse.wazaabi.engine.gwt.events.GWTEventAdapterFactory;
import org.eclipse.wazaabi.engine.gwt.events.GWTEventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.gwt.views.GWTWidgetViewFactory;


public class GWTHelper {

    public static void init(Registry registry) {
        CoreHelper.init(registry);

        EDPHelper.addService(registry, EditPartFactory.class, new GWTEditPartFactory());
        EDPHelper.addService(registry, WidgetViewFactory.class, new GWTWidgetViewFactory());
        EDPHelper.addService(registry, EventHandlerAdapterFactory.class, new GWTEventHandlerAdapterFactory());
        EDPHelper.addService(registry, EventAdapterFactory.class, new GWTEventAdapterFactory());
    }
}
