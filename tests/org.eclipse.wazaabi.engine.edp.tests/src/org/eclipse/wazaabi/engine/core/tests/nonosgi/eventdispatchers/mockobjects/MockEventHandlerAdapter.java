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

package org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.edp.adapters.EventDispatcherAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.mm.edp.events.Event;

public class MockEventHandlerAdapter extends EventHandlerAdapter {

	private List<Event> managedEvents = new ArrayList<Event>();
	private EventDispatcherAdapter attachedDispatcherAdapter = null;

	public EventDispatcherAdapter getAttachedDispatcherAdapter() {
		return attachedDispatcherAdapter;
	}

	@Override
	protected void eventAdded(Event event) {
		if (event != null)
			managedEvents.add(event);
		else
			fail();
	}

	@Override
	protected void eventRemoved(Event event) {
		if (event != null)
			managedEvents.remove(event);
		else
			fail();
	}

	public int getManagedEventCount(Event event) {
		int count = 0;
		for (Event item : managedEvents)
			if (event == item)
				count++;
		return count;
	}

	@Override
	protected void eventDispatcherAdapterAttached(
			EventDispatcherAdapter eventDispatcherAdapter) {
		this.attachedDispatcherAdapter = eventDispatcherAdapter;
	}

	@Override
	protected void eventDispatcherAdapterDetached(
			EventDispatcherAdapter eventDispatcherAdapter) {
		assertEquals(this.attachedDispatcherAdapter, eventDispatcherAdapter);
		this.attachedDispatcherAdapter = null;
	}

}
