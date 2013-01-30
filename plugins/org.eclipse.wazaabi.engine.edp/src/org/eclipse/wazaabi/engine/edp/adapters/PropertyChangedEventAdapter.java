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

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;

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
					e.printStackTrace();
					System.err.println(e.getErrorMessage());
				} catch (RuntimeException ex) {
					ex.printStackTrace();
				}
			}
		}

		public void setFeature(EStructuralFeature feature) {
			this.feature = feature;
		}

		@Override
		public boolean equals(Object other) {
			if (other instanceof FeatureAdapter
					&& ((FeatureAdapter) other).getTarget() != null
					&& ((FeatureAdapter) other).getTarget().equals(getTarget())
					&& ((FeatureAdapter) other).feature.equals(feature))
				return true;
			return false;
		}
	};

	public boolean isAdapterForType(Object type) {
		return PropertyChangedEvent.class.equals(type);
	}

	protected Adapter createAdapter(EObject target) {
		FeatureAdapter adapter = new FeatureAdapter();
		for (Adapter existingAdapter : getAdapters())
			if (existingAdapter instanceof FeatureAdapter
					&& existingAdapter.equals(adapter))
				return existingAdapter;
		((EObject) target).eAdapters().add(adapter);
		getAdapters().add(adapter);
		return adapter;
	}

	protected void adaptPointer(Object pointer,
			final IPointersEvaluator pointersEvalutator) {
		Object target = pointersEvalutator.getContext(pointer);
		if (target instanceof EObject) {
			String propertyName = pointersEvalutator.getPropertyName(pointer);
			if (propertyName != null) {
				EStructuralFeature feature = ((EObject) target).eClass()
						.getEStructuralFeature(propertyName);
				if (feature != null) {
					FeatureAdapter adapter = (FeatureAdapter) createAdapter((EObject) target);
					adapter.setFeature(feature);
				}
			}
		}
	}

}
