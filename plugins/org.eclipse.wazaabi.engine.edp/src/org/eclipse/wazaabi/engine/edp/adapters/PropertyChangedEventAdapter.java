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

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.PathException;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PathEvent;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class PropertyChangedEventAdapter extends AbstractPathEventAdapter {
	protected class FeatureAdapter extends AdapterImpl {

		private EStructuralFeature feature = null;

		@Override
		public void notifyChanged(Notification msg) {
			if (feature.equals(msg.getFeature())) {
				try {
					getEventHandlerAdapter().trigger(
							(Event) PropertyChangedEventAdapter.this
									.getTarget());
				} catch (OperationAborted e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					System.err.println(e.getErrorMessage());
				}
			}
		}

		public void setFeature(EStructuralFeature feature) {
			this.feature = feature;
		}

	};

	private List<FeatureAdapter> featureAdapters = new ArrayList<FeatureAdapter>(
			10);

	public boolean isAdapterForType(Object type) {
		return PropertyChangedEvent.class.equals(type);
	}

	// public void notifyChanged(Notification notification) {
	// if (notification.getEventType() == Notification.SET) {
	// switch (notification.getFeatureID(PropertyChangedEvent.class)) {
	// case EdpPackage.PROPERTY_CHANGED_EVENT__PATH:
	// // TODO test if the new event is not equals to the previous one
	// if (notification.getOldValue() != null)
	// stopPropertyChangedListening((PropertyChangedEvent) notification
	// .getOldValue());
	// if (notification.getNewValue() != null)
	// startPropertyChangedListening((PropertyChangedEvent) notification
	// .getNewValue());
	// break;
	// }
	// }
	// super.notifyChanged(notification);
	// }
	//
	// @Override
	// public void setTarget(Notifier newTarget) {
	// if (getTarget() instanceof PropertyChangedEvent)
	// stopPropertyChangedListening((PropertyChangedEvent) getTarget());
	// super.setTarget(newTarget);
	// if (getTarget() instanceof PropertyChangedEvent)
	// startPropertyChangedListening((PropertyChangedEvent) getTarget());
	// }

	@SuppressWarnings("rawtypes")
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
						List newPointers = pointersEvalutator.selectPointers(
								eventDispatcher, pathEvent.getPath());

						Iterator newPointersIterator = newPointers.iterator();
						while (newPointersIterator.hasNext()) {
							Object pointer = newPointersIterator.next();
							Object target = pointersEvalutator
									.getContext(pointer);
							if (target instanceof EObject) {
								String propertyName = pointersEvalutator
										.getPropertyName(pointer);
								if (propertyName != null) {
									EStructuralFeature feature = ((EObject) target)
											.eClass().getEStructuralFeature(
													propertyName);
									if (feature != null) {
										FeatureAdapter adapter = new FeatureAdapter();
										adapter.setFeature(feature);
										((EObject) target).eAdapters().add(
												adapter);
										featureAdapters.add(adapter);
									}
								}
							}
						}
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
		Iterator<FeatureAdapter> iterator = featureAdapters.iterator();
		while (iterator.hasNext()) {
			FeatureAdapter adapter = iterator.next();
			Notifier target = adapter.getTarget();
			if (target != null)
				target.eAdapters().remove(adapter);
		}
		featureAdapters.clear();
	}

	// public void attachListeners() {
	// if (getTarget() instanceof PropertyChangedEvent)
	// startPropertyChangedListening((PropertyChangedEvent) getTarget());
	// }
	//
	// public void detachListeners() {
	// if (getTarget() instanceof PropertyChangedEvent)
	// stopPropertyChangedListening((PropertyChangedEvent) getTarget());
	// }
}
