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
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.PathException;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;
import org.eclipse.wazaabi.mm.edp.events.PathEvent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public abstract class AbstractPathEventAdapter extends EventAdapter {

	private List<Adapter> adapters = new ArrayList<Adapter>(10);

	public boolean isAdapterForType(Object type) {
		return false;
	}

	public void notifyChanged(Notification notification) {
		if (notification.getEventType() == Notification.SET) {
			switch (notification.getFeatureID(PathEvent.class)) {
			case EDPEventsPackage.PATH_EVENT__PATH:

				// TODO we should test if the new event is not equals to the
				// previous one

				if (notification.getOldStringValue() != null
						&& !"".equals(notification.getOldStringValue())) { //$NON-NLS-1$
					// TODO : we should be able to avoid the duplicate of the
					// current PathEvent
					// by changing the signature of the xxxEventListening
					// methods
					PathEvent oldPathEvent = (PathEvent) EcoreUtil
							.copy((PathEvent) getTarget());
					oldPathEvent.setPath(notification.getOldStringValue());
					stopEventListening(oldPathEvent);
				}
				if (notification.getNewValue() != null
						&& !"".equals(notification.getNewStringValue())) //$NON-NLS-1$
					startEventListening((PathEvent) getTarget());
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

	public void attachListeners() {
		if (getTarget() instanceof PathEvent)
			startEventListening((PathEvent) getTarget());
	}

	public void detachListeners() {
		if (getTarget() instanceof PathEvent)
			stopEventListening((PathEvent) getTarget());
	}

	protected abstract Adapter createAdapter(EObject target);

	protected void startEventListening(PathEvent pathEvent) {
		if (pathEvent.eContainer() instanceof EventHandler
				&& pathEvent.eContainer().eContainer() instanceof EventDispatcher) {
			EventDispatcher eventDispatcher = (EventDispatcher) pathEvent
					.eContainer().eContainer();
			if (eventDispatcher != null && EDPSingletons.getRegistry() != null
					&& pathEvent.getPath() != null
					&& !"".equals(pathEvent.getPath())) { //$NON-NLS-1$

				final IPointersEvaluator pointersEvalutator = getPointersEvaluator();

				if (pointersEvalutator != null) {
					try {
						List<?> newPointers = pointersEvalutator
								.selectPointers(eventDispatcher,
										pathEvent.getPath());

						Iterator<?> newPointersIterator = newPointers
								.iterator();
						while (newPointersIterator.hasNext())
							adaptPointer(newPointersIterator.next(),
									pointersEvalutator);
					} catch (PathException e) {
						System.err.println(e.getMessage()); // TODO : log that
					}

				} else {
					// pointersEvaluator == null ===> log this
					assert false;
				}
			}
		}
	}

	protected void stopEventListening(PathEvent pathEvent) {
		for (Adapter adapter : getAdapters())
			unsetTarget(adapter);
		getAdapters().clear();
	}

	protected void unsetTarget(Adapter adapter) {
		Notifier target = adapter.getTarget();
		if (target != null)
			target.eAdapters().remove(adapter);
	}

	protected List<Adapter> getAdapters() {
		return adapters;
	}

	protected abstract void adaptPointer(Object pointer,
			final IPointersEvaluator pointersEvalutator);

}
