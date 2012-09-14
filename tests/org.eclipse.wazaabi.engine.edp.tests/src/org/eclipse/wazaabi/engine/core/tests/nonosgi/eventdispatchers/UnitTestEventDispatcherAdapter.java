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

import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventDispatcher;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventDispatcherAdapter;
import org.eclipse.wazaabi.engine.core.tests.nonosgi.eventdispatchers.mockobjects.MockEventHandler;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.tests.EMFUtils;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.junit.Test;

public class UnitTestEventDispatcherAdapter {

	@Test
	public void testAdaptEventHandler() {
		MockEventDispatcherAdapter eventDispatcherAdapter = new MockEventDispatcherAdapter();
		EventDispatcher eventDispatcher = new MockEventDispatcher();

		MockEventHandler eventHandler1 = new MockEventHandler();
		MockEventHandler eventHandler2 = new MockEventHandler();

		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler1,
				EventHandlerAdapter.class));
		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler2,
				EventHandlerAdapter.class));

		eventDispatcher.getHandlers().add(eventHandler1);

		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler1,
				EventHandlerAdapter.class));
		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler2,
				EventHandlerAdapter.class));

		eventDispatcher.eAdapters().add(eventDispatcherAdapter);

		assertEquals(1, EMFUtils.coundAdaptersOfTypeFor(eventHandler1,
				EventHandlerAdapter.class));
		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler2,
				EventHandlerAdapter.class));

		eventDispatcher.getHandlers().add(eventHandler2);

		assertEquals(1, EMFUtils.coundAdaptersOfTypeFor(eventHandler1,
				EventHandlerAdapter.class));
		assertEquals(1, EMFUtils.coundAdaptersOfTypeFor(eventHandler2,
				EventHandlerAdapter.class));

	}

	@Test
	public void testUnadaptEventHandler() {
		MockEventDispatcherAdapter eventDispatcherAdapter = new MockEventDispatcherAdapter();
		EventDispatcher eventDispatcher = new MockEventDispatcher();
		MockEventHandler eventHandler1 = new MockEventHandler();
		MockEventHandler eventHandler2 = new MockEventHandler();

		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler1,
				EventHandlerAdapter.class));
		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler2,
				EventHandlerAdapter.class));
		eventDispatcher.getHandlers().add(eventHandler1);
		eventDispatcher.getHandlers().add(eventHandler2);
		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler1,
				EventHandlerAdapter.class));
		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler2,
				EventHandlerAdapter.class));
		eventDispatcher.eAdapters().add(eventDispatcherAdapter);

		assertEquals(1, EMFUtils.coundAdaptersOfTypeFor(eventHandler1,
				EventHandlerAdapter.class));
		assertEquals(1, EMFUtils.coundAdaptersOfTypeFor(eventHandler2,
				EventHandlerAdapter.class));

		eventDispatcher.getHandlers().remove(eventHandler1);

		assertEquals(0, EMFUtils.coundAdaptersOfTypeFor(eventHandler1,
				EventHandlerAdapter.class));
		assertEquals(1, EMFUtils.coundAdaptersOfTypeFor(eventHandler2,
				EventHandlerAdapter.class));

	}

	@Test
	public void testEventHandlerAdded() {
		MockEventDispatcherAdapter eventDispatcherAdapter = new MockEventDispatcherAdapter();
		EventDispatcher eventDispatcher = new MockEventDispatcher();
		MockEventHandler eventHandler1 = new MockEventHandler();
		MockEventHandler eventHandler2 = new MockEventHandler();
		assertEquals(0,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler1));
		eventDispatcher.getHandlers().add(eventHandler1);
		assertEquals(0,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler1));
		eventDispatcher.eAdapters().add(eventDispatcherAdapter);
		assertEquals(1,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler1));
		assertEquals(0,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler2));
		eventDispatcher.getHandlers().add(eventHandler2);
		assertEquals(1,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler2));
	}

	@Test
	public void testEventHandlerRemoved() {
		MockEventDispatcherAdapter eventDispatcherAdapter = new MockEventDispatcherAdapter();
		EventDispatcher eventDispatcher = new MockEventDispatcher();
		MockEventHandler eventHandler1 = new MockEventHandler();
		MockEventHandler eventHandler2 = new MockEventHandler();

		eventDispatcher.getHandlers().add(eventHandler1);
		eventDispatcher.getHandlers().add(eventHandler2);
		eventDispatcher.eAdapters().add(eventDispatcherAdapter);

		assertEquals(1,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler1));
		assertEquals(1,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler2));
		eventDispatcher.getHandlers().remove(eventHandler1);
		assertEquals(0,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler1));
		assertEquals(1,
				eventDispatcherAdapter
						.getManagedEventHandlerCount(eventHandler2));
	}

}
