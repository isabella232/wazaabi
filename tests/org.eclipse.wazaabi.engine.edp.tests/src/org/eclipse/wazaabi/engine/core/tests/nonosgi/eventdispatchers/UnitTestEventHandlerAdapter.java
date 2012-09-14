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

package org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventDispatcher;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventDispatcherAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventHandler;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.tests.EMFUtils;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.junit.Test;

public class UnitTestEventHandlerAdapter {

	@Test
	public void testAdaptEvent() {
		MockEventHandlerAdapter eventHandlerAdapter = new MockEventHandlerAdapter();
		MockEventHandler eventHandler = new MockEventHandler();

		PropertyChangedEvent event1 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		PropertyChangedEvent event2 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();

		eventHandler.getEvents().add(event1);
		assertEquals(0,
				EMFUtils.coundAdaptersOfTypeFor(event1, EventAdapter.class));
		assertEquals(0,
				EMFUtils.coundAdaptersOfTypeFor(event2, EventAdapter.class));
		eventHandler.eAdapters().add(eventHandlerAdapter);
		assertEquals(1,
				EMFUtils.coundAdaptersOfTypeFor(event1, EventAdapter.class));
		assertEquals(0,
				EMFUtils.coundAdaptersOfTypeFor(event2, EventAdapter.class));

		assertNotNull(EMFUtils.getAdapterOfTypeFor(event1,
				MockEventAdapter.class));
		assertEquals(eventHandlerAdapter,
				((MockEventAdapter) EMFUtils.getAdapterOfTypeFor(event1,
						MockEventAdapter.class)).getEventHandlerAdapter());
		assertNull(EMFUtils.getAdapterOfTypeFor(event2, MockEventAdapter.class));
		eventHandler.getEvents().add(event2);
		assertNotNull(EMFUtils.getAdapterOfTypeFor(event2,
				MockEventAdapter.class));
		assertEquals(eventHandlerAdapter,
				((MockEventAdapter) EMFUtils.getAdapterOfTypeFor(event2,
						MockEventAdapter.class)).getEventHandlerAdapter());

		assertEquals(1,
				EMFUtils.coundAdaptersOfTypeFor(event1, EventAdapter.class));
		assertEquals(1,
				EMFUtils.coundAdaptersOfTypeFor(event2, EventAdapter.class));

	}

	@Test
	public void testUnadaptEvent() {
		MockEventHandlerAdapter eventHandlerAdapter = new MockEventHandlerAdapter();
		MockEventHandler eventHandler = new MockEventHandler();

		PropertyChangedEvent event1 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		PropertyChangedEvent event2 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();

		assertEquals(0,
				EMFUtils.coundAdaptersOfTypeFor(event1, EventAdapter.class));
		assertEquals(0,
				EMFUtils.coundAdaptersOfTypeFor(event2, EventAdapter.class));
		eventHandler.getEvents().add(event1);
		eventHandler.getEvents().add(event2);
		assertEquals(0,
				EMFUtils.coundAdaptersOfTypeFor(event1, EventAdapter.class));
		assertEquals(0,
				EMFUtils.coundAdaptersOfTypeFor(event2, EventAdapter.class));
		eventHandler.eAdapters().add(eventHandlerAdapter);
		assertEquals(1,
				EMFUtils.coundAdaptersOfTypeFor(event1, EventAdapter.class));
		assertEquals(1,
				EMFUtils.coundAdaptersOfTypeFor(event2, EventAdapter.class));
		eventHandler.getEvents().remove(event2);
		assertEquals(1,
				EMFUtils.coundAdaptersOfTypeFor(event1, EventAdapter.class));
		assertEquals(0,
				EMFUtils.coundAdaptersOfTypeFor(event2, EventAdapter.class));
	}

	@Test
	public void testEventAdded() {

		MockEventHandlerAdapter eventHandlerAdapter = new MockEventHandlerAdapter();
		MockEventHandler eventHandler = new MockEventHandler();

		PropertyChangedEvent event1 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		PropertyChangedEvent event2 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();

		assertEquals(0, eventHandlerAdapter.getManagedEventCount(event1));
		eventHandler.getEvents().add(event1);
		assertEquals(0, eventHandlerAdapter.getManagedEventCount(event1));

		eventHandler.eAdapters().add(eventHandlerAdapter);
		assertEquals(1, eventHandlerAdapter.getManagedEventCount(event1));

		assertEquals(0, eventHandlerAdapter.getManagedEventCount(event2));
		eventHandler.getEvents().add(event2);
		assertEquals(1, eventHandlerAdapter.getManagedEventCount(event2));

	}

	@Test
	public void testEventRemoved() {
		MockEventHandlerAdapter eventHandlerAdapter = new MockEventHandlerAdapter();
		MockEventHandler eventHandler = new MockEventHandler();

		PropertyChangedEvent event1 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		PropertyChangedEvent event2 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();

		eventHandler.getEvents().add(event1);
		eventHandler.getEvents().add(event2);
		eventHandler.eAdapters().add(eventHandlerAdapter);
		assertEquals(1, eventHandlerAdapter.getManagedEventCount(event1));
		assertEquals(1, eventHandlerAdapter.getManagedEventCount(event2));

		assertEquals(1,
				EMFUtils.coundAdaptersOfTypeFor(event1, EventAdapter.class));
		assertEquals(1,
				EMFUtils.coundAdaptersOfTypeFor(event2, EventAdapter.class));

		eventHandler.getEvents().remove(event2);

		assertEquals(1, eventHandlerAdapter.getManagedEventCount(event1));
		assertEquals(0, eventHandlerAdapter.getManagedEventCount(event2));
	}

	@Test
	public void testEventDispatcherAdapterAttached() {
		// test#1
		MockEventDispatcher eventDispatcher = new MockEventDispatcher();
		MockEventHandler eventHandler = new MockEventHandler();
		MockEventDispatcherAdapter eventDispatcherAdapter = new MockEventDispatcherAdapter();

		eventDispatcher.getHandlers().add(eventHandler);
		eventDispatcher.eAdapters().add(eventDispatcherAdapter);
		Adapter adapter = EMFUtils.getAdapterOfTypeFor(eventHandler,
				MockEventHandlerAdapter.class);
		assertTrue(adapter instanceof MockEventHandlerAdapter);
		assertEquals(eventDispatcherAdapter,
				((MockEventHandlerAdapter) adapter)
						.getAttachedDispatcherAdapter());

		// test#2
		MockEventDispatcher eventDispatcher2 = new MockEventDispatcher();
		MockEventDispatcherAdapter eventDispatcherAdapter2 = new MockEventDispatcherAdapter();

		eventDispatcher2.eAdapters().add(eventDispatcherAdapter2);
		eventDispatcher2.getHandlers().add(eventHandler);

		adapter = EMFUtils.getAdapterOfTypeFor(eventHandler,
				MockEventHandlerAdapter.class);
		assertTrue(adapter instanceof MockEventHandlerAdapter);
		assertEquals(eventDispatcherAdapter2,
				((MockEventHandlerAdapter) adapter)
						.getAttachedDispatcherAdapter());

		// test#2
		MockEventDispatcher eventDispatcher3 = new MockEventDispatcher();
		MockEventDispatcherAdapter eventDispatcherAdapter3 = new MockEventDispatcherAdapter();
		eventDispatcher3.eAdapters().add(eventDispatcherAdapter3);
		eventDispatcher3.getHandlers().add(eventHandler);
		adapter = EMFUtils.getAdapterOfTypeFor(eventHandler,
				MockEventHandlerAdapter.class);
		assertEquals(eventDispatcherAdapter3,
				((MockEventHandlerAdapter) adapter)
						.getAttachedDispatcherAdapter());

	}

	@Test
	public void testEventDispatcherAdapterDetached() {
		// test#1
		MockEventDispatcher eventDispatcher = new MockEventDispatcher();
		MockEventHandler eventHandler = new MockEventHandler();
		MockEventDispatcherAdapter eventDispatcherAdapter = new MockEventDispatcherAdapter();

		eventDispatcher.getHandlers().add(eventHandler);
		eventDispatcher.eAdapters().add(eventDispatcherAdapter);

		Adapter adapter = EMFUtils.getAdapterOfTypeFor(eventHandler,
				MockEventHandlerAdapter.class);
		assertTrue(adapter instanceof MockEventHandlerAdapter);
		assertEquals(eventDispatcherAdapter,
				((MockEventHandlerAdapter) adapter)
						.getAttachedDispatcherAdapter());
		adapter = EMFUtils.getAdapterOfTypeFor(eventHandler,
				MockEventHandlerAdapter.class);
		assertTrue(adapter instanceof MockEventHandlerAdapter);
		assertEquals(eventDispatcherAdapter,
				((MockEventHandlerAdapter) adapter)
						.getAttachedDispatcherAdapter());
		eventDispatcher.getHandlers().remove(eventHandler);
		assertEquals(null,
				((MockEventHandlerAdapter) adapter)
						.getAttachedDispatcherAdapter());
	}

	@Test
	public void testEventPathModified() {
		fail("Not yet implemented");
	}

}
