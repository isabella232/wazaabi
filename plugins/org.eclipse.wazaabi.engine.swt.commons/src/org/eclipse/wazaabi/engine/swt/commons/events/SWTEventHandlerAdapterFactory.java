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

package org.eclipse.wazaabi.engine.swt.commons.events;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class SWTEventHandlerAdapterFactory implements
		EventHandlerAdapterFactory {

	public boolean isFactoryFor(Object context, Object source) {
		// if (source instanceof EventHandler) {
		// System.out.println("isFactoryFor " + source);
		// return true;
		// }
		// This factory is not implemented/used at the moment
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	public EventHandlerAdapter createEventHandlerAdapter(Object context,
			EventHandler eventHandler) {
		if (context instanceof AbstractWidgetEditPart.InnerEventDispatcherAdapter)
			return new EventHandlerAdapter();
		return null;
	}

	public Adapter createAdapter(Object context, EObject model, Object creationHint) {
		if (context instanceof AbstractWidgetEditPart.InnerEventDispatcherAdapter)
			return new EventHandlerAdapter();
		return null;
	}

}
