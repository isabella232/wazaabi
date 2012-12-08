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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.DeferredAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;
import org.eclipse.wazaabi.mm.edp.handlers.impl.ConditionImpl;

public class EventHandlerAdapter extends ActionAdapterImpl implements
		SequenceAdapter {

	private SequenceAdapterImpl innerSequenceAdapter = new SequenceAdapterImpl() {

		@Override
		public void setTarget(Notifier newTarget) {
			Notifier oldTarget = getTarget();
			super.setTarget(newTarget);
			if (newTarget != null && newTarget.equals(oldTarget)
					&& getEventDispatcherAdapter() != null)
				updateCodeLocatorBaseUris(getEventDispatcherAdapter()
						.getCodeLocatorBaseUri());
		}

		@Override
		protected ExecutableAdapter createExecutableAdapterFor(
				Executable executable) {
			ExecutableAdapter adapter = super
					.createExecutableAdapterFor(executable);
			if (adapter instanceof DeferredAdapter)
				((DeferredAdapter) adapter)
						.setCodeLocatorBaseUri(getCodeLocatorBaseUri());
			return adapter;
		}

	};

	protected void eventAdded(Event event) {
	}

	protected void eventRemoved(Event event) {
	}

	protected void eventDispatcherAdapterAttached(
			EventDispatcherAdapter eventDispatcherAdapter) {
	}

	protected void eventDispatcherAdapterDetached(
			EventDispatcherAdapter eventDispatcherAdapter) {
	}

	private EventDispatcherAdapter eventDispatcherAdapter = null;

	private List<ConditionAdapter> conditionAdapters = new ArrayList<ConditionAdapter>();

	public EventDispatcherAdapter getEventDispatcherAdapter() {
		return eventDispatcherAdapter;
	}

	protected void setEventDispatcherAdapter(
			EventDispatcherAdapter eventDispatcherAdapter) {
		if (getEventDispatcherAdapter() != null)
			eventDispatcherAdapterDetached(getEventDispatcherAdapter());
		this.eventDispatcherAdapter = eventDispatcherAdapter;
		if (eventDispatcherAdapter != null) {
			eventDispatcherAdapterAttached(eventDispatcherAdapter);
			String newPathPrefix = eventDispatcherAdapter
					.getCodeLocatorBaseUri();
			innerSequenceAdapter.updateCodeLocatorBaseUris(newPathPrefix);
			setCodeLocatorBaseUri(newPathPrefix);
		} else {
			innerSequenceAdapter.updateCodeLocatorBaseUris(null);
			setCodeLocatorBaseUri(null);
		}
		super.registerMethods();
	}

	public void notifyChanged(Notification notification) {
		switch (notification.getFeatureID(EventHandler.class)) {
		case EDPHandlersPackage.EVENT_HANDLER__EVENTS:
			switch (notification.getEventType()) {
			case Notification.ADD:
				adaptEvent((Event) notification.getNewValue());
				break;
			case Notification.ADD_MANY:
				@SuppressWarnings("unchecked")
				Collection<Event> addedEvents = (Collection<Event>) notification
						.getNewValue();
				for (Event event : addedEvents)
					adaptEvent(event);
				break;
			case Notification.REMOVE:
				unadaptEvent((Event) notification.getOldValue());
				break;
			case Notification.REMOVE_MANY:
				@SuppressWarnings("unchecked")
				Collection<Event> removedEvents = (Collection<Event>) notification
						.getOldValue();
				for (Event event : removedEvents)
					unadaptEvent(event);
				break;
			}
			return;
		case EDPHandlersPackage.EVENT_HANDLER__CONDITIONS:
			switch (notification.getEventType()) {
			case Notification.ADD:
				adaptCondition((Condition) notification.getNewValue());
				break;
			case Notification.ADD_MANY:
				@SuppressWarnings("unchecked")
				Collection<ConditionImpl> addedConditions = (Collection<ConditionImpl>) notification
						.getNewValue();
				for (ConditionImpl condition : addedConditions)
					adaptCondition(condition);
				break;
			case Notification.REMOVE:
				unadaptCondition((Condition) notification.getOldValue());
				break;
			case Notification.REMOVE_MANY:
				@SuppressWarnings("unchecked")
				Collection<ConditionImpl> removedConditions = (Collection<ConditionImpl>) notification
						.getOldValue();
				for (ConditionImpl condition : removedConditions)
					unadaptCondition(condition);
				break;
			case Notification.SET:
				unadaptCondition((Condition) notification.getOldValue());
				adaptCondition((Condition) notification.getNewValue());
			}
		}
		innerSequenceAdapter.notifyChanged(notification);
		super.notifyChanged(notification);
	}

	@Override
	public void setTarget(Notifier newTarget) {
		innerSequenceAdapter.setTarget(newTarget);
		if (newTarget == getTarget())
			return;
		if (getTarget() != null) {
			for (Event event : ((EventHandler) getTarget()).getEvents()) {
				unadaptEvent(event);
			}
			for (Condition condition : ((EventHandler) getTarget())
					.getConditions()) {
				unadaptCondition(condition);
			}
		}
		super.setTarget(newTarget);
		if (newTarget != null) {
			for (Event event : ((EventHandler) getTarget()).getEvents()) {
				adaptEvent(event);
			}
			for (Condition condition : ((EventHandler) getTarget())
					.getConditions()) {
				adaptCondition(condition);
			}
		}
	}

	protected void adaptEvent(Event event) {
		EventAdapter adapter = createEventAdapterFor(event);
		if (adapter != null) {
			adapter.setEventHandlerAdapter(this);
			event.eAdapters().add(adapter);
		}
		eventAdded(event);
	}

	protected void unadaptEvent(Event event) {
		EventAdapter toRemove = null;
		for (Adapter adapter : event.eAdapters())
			if (adapter instanceof EventAdapter
					&& ((EventAdapter) adapter).getEventHandlerAdapter() == this) {
				toRemove = (EventAdapter) adapter;
				break;
			}
		if (toRemove != null) {
			event.eAdapters().remove(toRemove);
			toRemove.setEventHandlerAdapter(null);
		}
		eventRemoved(event);
	}

	protected EventAdapter createEventAdapterFor(Event event) {
		if (EDPSingletons.getComposedEventAdapterFactory() != null) {
			return EDPSingletons.getComposedEventAdapterFactory()
					.createEventAdapter(this, event);
		}
		return null;
	}

	protected void adaptCondition(Condition condition) {
		ConditionAdapter conditionAdapter = (ConditionAdapter) createConditionAdapterFor(condition);
		if (conditionAdapter != null) {
			condition.eAdapters().add(conditionAdapter);
			getConditionAdapters().add(conditionAdapter);
		}
	}

	private void unadaptCondition(Condition condition) {
		ConditionAdapter toRemove = null;
		for (Adapter adapter : condition.eAdapters())
			if (adapter instanceof ConditionAdapter
					&& getConditionAdapters().contains(
							((ConditionAdapter) adapter))) {
				toRemove = (ConditionAdapter) adapter;
				break;
			}
		if (toRemove != null) {
			condition.eAdapters().remove(toRemove);
			getConditionAdapters().remove(toRemove);
		}
	}

	protected ConditionAdapter createConditionAdapterFor(Condition condition) {
		ExecutableAdapter adapter = null;
		if (EDPSingletons.getComposedExecutableAdapterFactory() != null) {
			adapter = EDPSingletons.getComposedExecutableAdapterFactory()
					.createExecutableAdapter(this, condition);
			if (adapter instanceof ConditionAdapter) {
				((ConditionAdapter) adapter)
						.setCodeLocatorBaseUri(getCodeLocatorBaseUri());
				return (ConditionAdapter) adapter;
			}
		}
		return null;
	}

	public List<ConditionAdapter> getConditionAdapters() {
		return conditionAdapters;
	}

	public void trigger(Event event) throws OperationAborted {
		EventDispatcher dispatcher = (EventDispatcher) ((EventHandler) getTarget())
				.eContainer();
		for (ConditionAdapter condition : getConditionAdapters()) {
			boolean canExecute = false;
			try {
				canExecute = condition.canExecute(dispatcher,
						(EventHandler) getTarget(), event);
			} catch (RuntimeException e) {
				throw (OperationAborted) e.getCause();
			}
			if (!canExecute) {
				return;
			}
		}
		if (((EventHandler) getTarget()).getExecutables().isEmpty())
			super.trigger(dispatcher, (EventHandler) getTarget(), event);
		else
			innerSequenceAdapter.trigger(dispatcher,
					(EventHandler) getTarget(), event);

	}

	public IPointersEvaluator getPointersEvaluator() {
		if (getEventDispatcherAdapter() != null)
			return getEventDispatcherAdapter().getPointersEvaluator();
		return null;
	}

	public List<ExecutableAdapter> getExecutableAdapters() {
		return innerSequenceAdapter.getExecutableAdapters();
	}

	@Override
	protected void registerMethods() {
		if (getEventDispatcherAdapter() != null)
			super.registerMethods();
	}

}
