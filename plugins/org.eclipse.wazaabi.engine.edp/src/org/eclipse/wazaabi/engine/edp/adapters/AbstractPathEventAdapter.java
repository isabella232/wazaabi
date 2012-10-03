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
import org.eclipse.emf.ecore.util.EcoreUtil;
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

				// TODO we should test if the new event is not equals to the previous one

				if (notification.getOldStringValue() != null
						&& !"".equals(notification.getOldStringValue())) { //$NON-NLS-1$
					//TODO : we should be able to avoid the duplicate of the current PathEvent
					// by changing the signature of the xxxEventListening methods
					PathEvent oldPathEvent = (PathEvent) EcoreUtil
							.copy((PathEvent) getTarget());
					oldPathEvent.setPath(notification.getOldStringValue());
					stopEventListening(oldPathEvent);
				}
				if (notification.getNewValue() != null
						&& !"".equals(notification.getNewStringValue())) //$NON-NLS-1$
					startEventListening((PathEvent)getTarget());
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

	protected abstract void startEventListening(PathEvent pathEvent);

	protected abstract void stopEventListening(PathEvent pathEvent);

	public void attachListeners() {
		if (getTarget() instanceof PathEvent)
			startEventListening((PathEvent) getTarget());
	}

	public void detachListeners() {
		if (getTarget() instanceof PathEvent)
			stopEventListening((PathEvent) getTarget());
	}
}
