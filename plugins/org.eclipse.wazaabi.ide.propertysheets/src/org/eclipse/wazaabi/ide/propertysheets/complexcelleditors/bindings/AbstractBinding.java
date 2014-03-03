/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.viewers.TargetChangeListener;

public abstract class AbstractBinding {

	protected static final String ESTRUCTURAL_FEATURE_KEY = "StructuralFeatureKey"; //$NON-NLS-1$
	protected static final String ADAPTER_KEY = "EAdapter"; // NON-NLS-1$
	protected static final String TARGET_CHANGED_LISTENER_KEY = "TargetChangeListener"; //$NON-NLS-1$

	protected abstract void addListeners(final Control control);

	public final void bind(final Control control,
			final EStructuralFeature feature,
			TargetChangeListener targetChangeListener) {
		if (control == null || control.isDisposed())
			return;
		control.setData(ESTRUCTURAL_FEATURE_KEY, feature);
		control.setData(TARGET_CHANGED_LISTENER_KEY, targetChangeListener);
		control.setData(ADAPTER_KEY, new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				if (msg.getFeature() == feature) {
					refresh(control);
				}
			}
		});
		control.addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {
				Adapter adapter = getAdapter(control);
				if (adapter != null && adapter.getTarget() != null)
					adapter.getTarget().eAdapters().remove(adapter);
			}
		});
		addListeners(control);
	}

	abstract protected Object convertToExpectedValue(Object value);

	protected final Adapter getAdapter(Control control) {
		if (control != null && !control.isDisposed())
			return (Adapter) control.getData(ADAPTER_KEY);
		return null;
	}

	protected final EObject getDomainObject(Control control) {
		Adapter adapter = getAdapter(control);
		if (adapter != null)
			return (EObject) adapter.getTarget();
		return null;
	}

	protected final Object getDomainValue(Control control) {
		EObject domainObject = getDomainObject(control);
		if (domainObject != null)
			return domainObject.eGet(getEStructuralFeature(control));
		return null;
	}

	protected String getErrorMessage(Object value) {
		return null;
	}

	protected final EStructuralFeature getEStructuralFeature(Control control) {
		return (EStructuralFeature) control.getData(ESTRUCTURAL_FEATURE_KEY);
	}

	protected TargetChangeListener getTargetChangeListener(Control control) {
		if (control != null && !control.isDisposed())
			return (TargetChangeListener) control
					.getData(TARGET_CHANGED_LISTENER_KEY);
		return null;
	}

	public abstract void refresh(Control control);

	public void setInput(final Control control, EObject newInput) {
		if (control == null || control.isDisposed())
			return;
		Adapter currentAdapter = getAdapter(control);
		if (newInput == currentAdapter.getTarget())
			return;
		if (currentAdapter.getTarget() != null)
			currentAdapter.getTarget().eAdapters().remove(currentAdapter);
		currentAdapter.setTarget(newInput);
	}
}
