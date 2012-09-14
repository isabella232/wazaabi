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

package org.eclipse.wazaabi.engine.core.events;

import org.eclipse.wazaabi.engine.core.adapters.RefreshActionAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;
import org.eclipse.wazaabi.mm.core.handlers.CoreHandlersPackage;
import org.eclipse.wazaabi.mm.core.handlers.RefreshAction;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class CoreEventHandlerAdapterFactory implements
		EventHandlerAdapterFactory {

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof EventHandler
				&& ((EventHandler) source).eClass().getEPackage() == CoreHandlersPackage.eINSTANCE)
			return true;
		return false;

	}

	public String getFactoryID() {
		return getClass().getName();
	}

	public EventHandlerAdapter createEventHandlerAdapter(
			Object context, EventHandler eventHandler) {
		if (eventHandler != null
				&& eventHandler.eClass().getEPackage() == CoreHandlersPackage.eINSTANCE) {
			if (eventHandler instanceof RefreshAction)
				return new RefreshActionAdapter();
		}
		return null;
	}

}
