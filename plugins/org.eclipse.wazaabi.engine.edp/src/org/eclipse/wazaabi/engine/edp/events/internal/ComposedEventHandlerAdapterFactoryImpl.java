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

package org.eclipse.wazaabi.engine.edp.events.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.events.ComposedEventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ComposedEventHandlerAdapterFactoryImpl implements
		ComposedEventHandlerAdapterFactory {

	private List<EventHandlerAdapterFactory> eventHandlerAdapterFactories = new ArrayList<EventHandlerAdapterFactory>();

	public void addEventHandlerAdapterFactory(EventHandlerAdapterFactory factory) {
		if (factory == null || factory.getFactoryID() == null
				|| "".equals(factory.getFactoryID())) //$NON-NLS-1$
			return;
		for (EventHandlerAdapterFactory item : eventHandlerAdapterFactories)
			if (item.getFactoryID().equals(factory.getFactoryID()))
				return;
//		System.out.println(":::: adding " + factory);
		eventHandlerAdapterFactories.add(factory);
	}

	public void removeEventHandlerAdapterFactory(EventHandlerAdapterFactory factory) {
//		System.out.println(":::: removing " + factory);
		eventHandlerAdapterFactories.remove(factory);
	}

	public EventHandlerAdapter createEventHandlerAdapter(Object context,
			EventHandler eventHandler) {
		if (eventHandler == null)
			return null;
		final EventHandlerAdapterFactory factory = getFactoryFor(context,
				eventHandler);
		if (factory != null)
			return factory.createEventHandlerAdapter(context, eventHandler);
		return null;
	}

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof EventHandler) {
			final EventHandlerAdapterFactory factory = getFactoryFor(context,
					(EventHandler) source);
			if (factory != null)
				return factory.isFactoryFor(context, source);
		}
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	protected EventHandlerAdapterFactory getFactoryFor(Object context,
			EventHandler eventHandler) {
		if (eventHandler == null)
			return null;
		for (EventHandlerAdapterFactory factory : eventHandlerAdapterFactories)
			if (factory.isFactoryFor(context, eventHandler))
				return factory;
		return null;
	}
}
