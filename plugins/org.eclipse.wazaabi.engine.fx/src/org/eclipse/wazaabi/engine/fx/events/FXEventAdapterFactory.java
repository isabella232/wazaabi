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

package org.eclipse.wazaabi.engine.fx.events;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.events.EventAdapterFactory;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.mm.edp.events.Event;


public class FXEventAdapterFactory implements EventAdapterFactory {

    public boolean isFactoryFor(Object context, Object source, Object creationHint) {
        return source instanceof Event && getFXViewer(context) instanceof FXViewer;
    }

    public String getFactoryID() {
        return getClass().getName();
    }

    public EventAdapter createEventAdapter(Object context, Event event) {
        // if (event is ui event) ...
        if (context instanceof EventHandlerAdapter
                && ((EventHandlerAdapter) context).getEventDispatcherAdapter() instanceof WidgetEditPart)
            return new FXEventAdapter();
        return null;
    }

    /** Returns the AbstractSWTControlViewer associated to this context (here, the context is supposed to be an
     * EventHandlerAdapter. Returns null if no viewer can be found.
     * 
     * @param context
     * @return */
    private FXViewer getFXViewer(Object context) {
        if (context instanceof EventHandlerAdapter
                && ((EventHandlerAdapter) context).getEventDispatcherAdapter() instanceof AbstractWidgetEditPart
                && ((AbstractWidgetEditPart) ((EventHandlerAdapter) context)
                        .getEventDispatcherAdapter()).getViewer() instanceof FXViewer)
            return (FXViewer) ((AbstractWidgetEditPart) ((EventHandlerAdapter) context)
                    .getEventDispatcherAdapter()).getViewer();
        return null;
    }

    public Adapter createAdapter(Object context, EObject model, Object creationHint) {
        if (context instanceof EventHandlerAdapter
                && ((EventHandlerAdapter) context).getEventDispatcherAdapter() instanceof WidgetEditPart)
            return new FXEventAdapter();
        return null;
    }
}
