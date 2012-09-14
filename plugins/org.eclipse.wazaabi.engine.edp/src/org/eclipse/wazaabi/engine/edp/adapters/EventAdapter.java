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

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;
import org.eclipse.wazaabi.mm.edp.events.Event;

public abstract class EventAdapter extends AdapterImpl {

	private EventHandlerAdapter eventHandlerAdapter = null;

	public EventHandlerAdapter getEventHandlerAdapter() {
		return eventHandlerAdapter;
	}

	protected void setEventHandlerAdapter(
			EventHandlerAdapter eventHandlerAdapter) {
		this.eventHandlerAdapter = eventHandlerAdapter;
	}

	@Override
	public boolean isAdapterForType(Object type) {
		return type instanceof Event;
	}

	@Override
	public void notifyChanged(Notification notification) {
		switch (notification.getFeatureID(Event.class)) {
		case EDPEventsPackage.EVENT__ID:
			if (!CompareUtils.areEquals(notification.getOldStringValue(),
					notification.getNewStringValue()))
				updateEventId(notification.getOldStringValue(),
						notification.getNewStringValue());
			break;
		}
	}

	protected void updateEventId(String oldId, String newId) {

	}

	public IPointersEvaluator getPointersEvaluator() {
		if (getEventHandlerAdapter() != null)
			return getEventHandlerAdapter().getPointersEvaluator();
		return null;
	}

}
