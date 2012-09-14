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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.wazaabi.engine.edp.adapters.EventDispatcherAdapterImpl;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.mm.edp.EdpPackage;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class MockEventDispatcherAdapter extends EventDispatcherAdapterImpl {

	private List<EventHandler> managedEventHandlers = new ArrayList<EventHandler>();

//	@Override
//	protected EventHandlerAdapter createEventHandlerAdapterFor(
//			EventHandler eventHandler) {
//		if (eventHandler instanceof MockEventHandler)
//			return new MockEventHandlerAdapter();
//		return super.createEventHandlerAdapterFor(eventHandler);
//	}
//
//	@Override
//	protected void eventHandlerAdded(EventHandler eventHandler) {
//		if (eventHandler != null)
//			managedEventHandlers.add(eventHandler);
//		else
//			fail();
//	}
//
//	@Override
//	protected void eventHandlerRemoved(EventHandler eventHandler) {
//		if (eventHandler != null)
//			managedEventHandlers.remove(eventHandler);
//		else
//			fail();
//	}
	
	@Override
	public void notifyChanged(Notification notification) {
		switch (notification.getFeatureID(EventDispatcher.class)) {
		case EdpPackage.EVENT_DISPATCHER__HANDLERS:
			switch (notification.getEventType()) {
			case Notification.ADD:
				adaptEventHandler((EventHandler) notification.getNewValue());
				break;
			case Notification.ADD_MANY:
				@SuppressWarnings("unchecked")
				Collection<EventHandler> addedEventHandlers = (Collection<EventHandler>) notification
						.getNewValue();
				for (EventHandler eventHandler : addedEventHandlers)
					adaptEventHandler(eventHandler);
				break;
			case Notification.REMOVE:
				unadaptEventHandler((EventHandler) notification.getOldValue());
				break;
			case Notification.REMOVE_MANY:
				@SuppressWarnings("unchecked")
				Collection<EventHandler> removedEventHandlers = (Collection<EventHandler>) notification
						.getOldValue();
				for (EventHandler eventHandler : removedEventHandlers)
					unadaptEventHandler(eventHandler);
				break;
			}
		}
	}

	public int getManagedEventHandlerCount(EventHandler eventHandler) {
		int count = 0;
		for (EventHandler item : managedEventHandlers)
			if (eventHandler == item)
				count++;
		return count;
	}

	public IPointersEvaluator getPointersEvaluator() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void eventHandlerAdded(EventHandler eventHandler) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void eventHandlerRemoved(EventHandler eventHandler) {
		// TODO Auto-generated method stub
		
	}
}
