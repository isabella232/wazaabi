/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
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

import java.awt.Container;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.mm.edp.Context;
import org.eclipse.wazaabi.mm.edp.ContextContent;
import org.eclipse.wazaabi.mm.edp.EdpPackage;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PathEvent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ContextAdapter extends AdapterImpl {

	@Override
	public void notifyChanged(Notification notification) {
		switch (notification
				.getFeatureID(org.eclipse.wazaabi.mm.edp.Context.class)) {
		case EdpPackage.CONTEXT__CONTENTS:
			switch (notification.getEventType()) {
			case Notification.ADD:
				hookContextContent((ContextContent) notification.getNewValue(),
						true);
				break;
			case Notification.REMOVE:
				unhookContextContent(
						(ContextContent) notification.getOldValue(), true);
				break;
			case Notification.ADD_MANY:
			case Notification.REMOVE_MANY:
				// ADD_MANY & REMOVE_MANY are not possible since addition and
				// removal are made through 'set(key, data)'
				throw new UnsupportedOperationException();
			}
		}
	}

	protected void updateRelatedEventHandlersFor(String entry, Object context,
			HashMap<EventHandler, List<PathEvent>> eventHandlers) {
		if (context instanceof EventDispatcher)
			for (EventHandler eventHandler : ((EventDispatcher) context)
					.getHandlers())
				for (Event event : eventHandler.getEvents())
					if (event instanceof PathEvent
							&& ((PathEvent) event).getPath() != null
							&& ((PathEvent) event).getPath().startsWith(entry)) {
						List<PathEvent> events = eventHandlers
								.get(eventHandler);
						if (events == null) {
							events = new ArrayList<PathEvent>();
							eventHandlers.put(eventHandler, events);
						}
						events.add((PathEvent) event);
						break;
					}
		if (context instanceof Container)
			for (AbstractComponent component : ((Container) context)
					.getChildren())
				updateRelatedEventHandlersFor(entry, component, eventHandlers);
	}

	@Override
	public void setTarget(Notifier newTarget) {
		if (getTarget() != null)
			for (ContextContent content : ((Context) getTarget()).getContents())
				unhookContextContent(content, false);
		super.setTarget(newTarget);
		if (getTarget() != null)
			for (ContextContent content : ((Context) getTarget()).getContents())
				hookContextContent(content, false);
	}

	protected void unhookContextContent(ContextContent content,
			boolean triggerAfterHook) {
		// we remove the existing ContextContentAdapter(s)
		List<ContextContentAdapter> toRemove = new ArrayList<ContextContentAdapter>();
		for (Adapter adapter : content.eAdapters())
			if (adapter instanceof ContextContentAdapter)
				toRemove.add((ContextContentAdapter) adapter);
		for (ContextContentAdapter adapter : toRemove)
			content.eAdapters().remove(adapter);
		if (content.getKey() == null || content.getKey().isEmpty())
			return;
		HashMap<EventHandler, List<PathEvent>> eventHandlers = new HashMap<EventHandler, List<PathEvent>>();
		updateRelatedEventHandlersFor("$" + content.getKey(), getTarget(),
				eventHandlers);
		// we create all the PathEventAdapters for this entry
		for (Entry<EventHandler, List<PathEvent>> entry : eventHandlers
				.entrySet())
			for (Adapter eventHandlerAdapter : entry.getKey().eAdapters())
				if (eventHandlerAdapter instanceof EventHandlerAdapter)
					for (Event event : entry.getValue())
						((EventHandlerAdapter) eventHandlerAdapter)
								.unadaptEvent(event);
		if (triggerAfterHook)
			// we trigger every relevant EventHandlerAdapter since an entry has
			// been removed
			for (Entry<EventHandler, List<PathEvent>> entry : eventHandlers
					.entrySet())
				for (Adapter eventHandlerAdapter : entry.getKey().eAdapters())
					if (eventHandlerAdapter instanceof EventHandlerAdapter)
						for (Event event : entry.getValue())
							((EventHandlerAdapter) eventHandlerAdapter)
									.trigger(event);

	}

	protected void hookContextContent(ContextContent content,
			boolean triggerAfterHook) {
		ContextContentAdapter adapter = new ContextContentAdapter();
		content.eAdapters().add(adapter);
		if (content.getKey() == null || content.getKey().isEmpty())
			return;
		HashMap<EventHandler, List<PathEvent>> eventHandlers = new HashMap<EventHandler, List<PathEvent>>();
		updateRelatedEventHandlersFor("$" + content.getKey(), getTarget(),
				eventHandlers);
		// we create all the PathEventAdapters for this entry
		for (Entry<EventHandler, List<PathEvent>> entry : eventHandlers
				.entrySet())
			for (Adapter eventHandlerAdapter : entry.getKey().eAdapters())
				if (eventHandlerAdapter instanceof EventHandlerAdapter)
					for (Event event : entry.getValue())
						((EventHandlerAdapter) eventHandlerAdapter)
								.adaptEvent(event);
		if (triggerAfterHook)
			// we trigger every relevant EventHandlerAdapter since an entry has
			// been added
			for (Entry<EventHandler, List<PathEvent>> entry : eventHandlers
					.entrySet())
				for (Adapter eventHandlerAdapter : entry.getKey().eAdapters())
					if (eventHandlerAdapter instanceof EventHandlerAdapter)
						for (Event event : entry.getValue())
							((EventHandlerAdapter) eventHandlerAdapter)
									.trigger(event);
	}

}
