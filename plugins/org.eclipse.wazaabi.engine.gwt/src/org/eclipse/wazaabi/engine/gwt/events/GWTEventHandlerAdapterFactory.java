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

package org.eclipse.wazaabi.engine.gwt.events;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;


public class GWTEventHandlerAdapterFactory implements EventHandlerAdapterFactory {

    public static final String FACTORY_ID = GWTEventHandlerAdapterFactory.class.getName();

    public boolean isFactoryFor(Object context, Object source, Object creationHint) {
        return context instanceof AbstractWidgetEditPart.InnerEventDispatcherAdapter;
    }

    public String getFactoryID() {
        return FACTORY_ID;
    }

    public Adapter createAdapter(Object context, EObject model, Object creationHint) {
        if (context instanceof AbstractWidgetEditPart.InnerEventDispatcherAdapter)
            return new EventHandlerAdapter();
        return null;
    }
}
