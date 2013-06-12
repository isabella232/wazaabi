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

package org.eclipse.wazaabi.engine.edp.adapters;

import java.util.Collection;
import java.util.HashSet;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.mm.edp.EdpPackage;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public abstract class EventDispatcherAdapterImpl extends AdapterImpl implements
		EventDispatcherAdapter {

	private HashSet<String> locks = new HashSet<String>();

	@Override
	public boolean isAdapterForType(Object type) {
		return type instanceof EventDispatcher;
	}

	protected EventDispatcherAdapter getEventDispatcherAdapter() {
		return this;
	}

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

	protected void adaptEventHandler(EventHandler eventHandler) {
		if (eventHandler == null)
			return;
		EventHandlerAdapter adapter = createEventHandlerAdapterFor(eventHandler);
		if (adapter != null) {
			adapter.setEventDispatcherAdapter(getEventDispatcherAdapter());
			eventHandler.eAdapters().add(adapter);
			eventHandlerAdded(eventHandler);
		}
	}

	protected void unadaptEventHandler(EventHandler eventHandler) {
		EventHandlerAdapter toRemove = null;
		for (Adapter adapter : eventHandler.eAdapters())
			if (adapter instanceof EventHandlerAdapter
					&& ((EventHandlerAdapter) adapter)
							.getEventDispatcherAdapter() == getEventDispatcherAdapter()) {
				toRemove = (EventHandlerAdapter) adapter;
				break;
			}
		if (toRemove != null) {
			eventHandler.eAdapters().remove(toRemove);
			toRemove.setEventDispatcherAdapter(null);
			eventHandlerRemoved(eventHandler);
		}
	}

	@Override
	public void setTarget(Notifier newTarget) {
		if (newTarget == getTarget())
			return;
		if (getTarget() != null)
			for (EventHandler eventHandler : ((EventDispatcher) getTarget())
					.getHandlers())
				unadaptEventHandler(eventHandler);
		super.setTarget(newTarget);
		if (newTarget != null)
			for (EventHandler eventHandler : ((EventDispatcher) newTarget)
					.getHandlers())
				adaptEventHandler(eventHandler);
	}

	protected EventHandlerAdapter createEventHandlerAdapterFor(
			EventHandler eventHandler) {

		return  (EventHandlerAdapter) getEDPFactory()
				.createAdapter(this, eventHandler, EventHandlerAdapter.class);
		// if (EDPSingletons.getComposedEventHandlerAdapterFactory() != null) {
		// return EDPSingletons.getComposedEventHandlerAdapterFactory()
		// .createEventHandlerAdapter(this, eventHandler);
		// }
		// return null;
	}

	protected abstract void eventHandlerAdded(EventHandler eventHandler);

	protected abstract void eventHandlerRemoved(EventHandler eventHandler);

	public void lock(String id) {
		if (id == null || "".equals(id)) //$NON-NLS-1$
			return;
		locks.add(id);
	}

	public void unlock(String id) {
		if (id == null || "".equals(id)) //$NON-NLS-1$
			return;
		locks.remove(id);
	}

	public boolean isLocked(String id) {
		if (id == null || "".equals(id)) //$NON-NLS-1$
			return false;
		return locks.contains(id);
	}

}
