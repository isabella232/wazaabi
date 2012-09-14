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

import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.events.ComposedEventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.EventAdapterFactory;

public class ComposedEventAdapterFactoryImpl implements
		ComposedEventAdapterFactory {

	private List<EventAdapterFactory> eventAdapterFactories = new ArrayList<EventAdapterFactory>();

	public void addEventAdapterFactory(EventAdapterFactory factory) {
		if (factory == null || factory.getFactoryID() == null
				|| "".equals(factory.getFactoryID())) //$NON-NLS-1$
			return;
		for (EventAdapterFactory item : eventAdapterFactories)
			if (item.getFactoryID().equals(factory.getFactoryID()))
				return;
//		System.out.println(":::: adding " + factory);
		eventAdapterFactories.add(factory);
	}

	public void removeEventAdapterFactory(EventAdapterFactory factory) {
//		System.out.println(":::: removing " + factory);
		eventAdapterFactories.remove(factory);
	}

	public EventAdapter createEventAdapter(Object context, Event event) {
		if (event == null)
			return null;
		final EventAdapterFactory factory = getFactoryFor(context, event);
		if (factory != null)
			return factory.createEventAdapter(context, event);
		return null;
	}

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof Event) {
			final EventAdapterFactory factory = getFactoryFor(context,
					(Event) source);
			if (factory != null)
				return factory.isFactoryFor(context, source);
		}
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	protected EventAdapterFactory getFactoryFor(Object context, Event event) {
		if (event == null)
			return null;
		for (EventAdapterFactory factory : eventAdapterFactories)
			if (factory.isFactoryFor(context, event))
				return factory;
		return null;
	}
}
