/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.events;

import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.events.EventAdapterFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;

public class SWTEventAdapterFactory implements EventAdapterFactory {

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof Event) {
//			System.out.println("isFactoryFor " + source);
			return true;
		}
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	public EventAdapter createEventAdapter(Object context, Event event) {
		// if (event is ui event) ...
		if (context instanceof EventHandlerAdapter
				&& ((EventHandlerAdapter) context).getEventDispatcherAdapter() instanceof WidgetEditPart)
			return new SWTUIEventAdapter();
		return null;
	}

}
