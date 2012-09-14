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
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;
import org.eclipse.wazaabi.mm.edp.events.PathEvent;

public abstract class AbstractPathEventAdapter extends EventAdapter {

	public boolean isAdapterForType(Object type) {
		return false;
	}

	public void notifyChanged(Notification notification) {
		if (notification.getEventType() == Notification.SET) {
			switch (notification.getFeatureID(PathEvent.class)) {
			case EDPEventsPackage.PATH_EVENT__PATH:
				// TODO test if the new event is not equals to the previous one
				if (notification.getOldValue() != null)
					stopEventListening((PathEvent) notification
							.getOldValue());
				if (notification.getNewValue() != null)
					startEventListening((PathEvent) notification
							.getNewValue());
				break;
			}
		}
		super.notifyChanged(notification);
	}

	@Override
	public void setTarget(Notifier newTarget) {
		if (getTarget() instanceof PathEvent)
			stopEventListening((PathEvent) getTarget());
		super.setTarget(newTarget);
		if (getTarget() instanceof PathEvent)
			startEventListening((PathEvent) getTarget());
	}

	protected abstract void startEventListening(
			PathEvent pathEvent);

	protected abstract void stopEventListening(
			PathEvent pathEvent);

	public void attachListeners() {
		if (getTarget() instanceof PathEvent)
			startEventListening((PathEvent) getTarget());
	}

	public void detachListeners() {
		if (getTarget() instanceof PathEvent)
			stopEventListening((PathEvent) getTarget());
	}
}
