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

package org.eclipse.wazaabi.ide.propertysheets.complexcelleditors;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeService;

public abstract class InPlaceCellEditor extends CellEditor implements
		TargetChangeService, TargetChangeListener {

	private List<TargetChangeListener> listeners = new ArrayList<TargetChangeListener>();
	private Object input = null;



	public InPlaceCellEditor(Composite parent) {
		super(parent);
	}

	public void addTargetChangeListener(TargetChangeListener listener) {
		listeners.add(listener);
	}

	@Override
	protected Control createControl(Composite parent) {
		Composite mainSection = createMainSection(parent);
		createSelectorSection(mainSection);
		createDetailsSection(mainSection);
		return mainSection;
	}

	abstract protected Control createDetailsSection(Composite parent);

	abstract protected Layout createLayout();

	abstract protected Composite createMainSection(Composite parent);

	abstract protected Control createSelectorSection(Composite parent);

	@Override
	protected Object doGetValue() {
		return null;
	}

	@Override
	protected void doSetFocus() {
	}

	@Override
	protected void doSetValue(Object value) {
		setInput(value);
		refresh();
	}

	protected void fireTargetAdded(EObject target, int position) {
		for (TargetChangeListener listener : listeners)
			listener.targetAdded(target, position);
	}

	protected void fireTargetModified(EObject target,
			EStructuralFeature feature, int position, Object oldValue,
			Object newValue) {
		for (TargetChangeListener listener : listeners)
			listener.targetModified(target, feature, position, oldValue,
					newValue);
	}

	protected void fireTargetModified(EObject target,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {
		for (TargetChangeListener listener : listeners)
			listener.targetMultipleModified(target, features, positions,
					oldValues, newValues);
	}

	protected void fireTargetRemoved(EObject target) {
		for (TargetChangeListener listener : listeners)
			listener.targetRemoved(target);
	}

	abstract protected String getHeaderTitle();

	public Object getInput() {
		return input;
	}

	public void refresh() {

	}

	public void removeTargetChangeListener(TargetChangeListener listener) {
		listeners.remove(listener);
	}

	protected void setInput(Object input) {
		this.input = input;
	}

	public void targetAdded(EObject target, int position) {
		fireTargetAdded(target, position);
	}

	public void targetModified(EObject target, EStructuralFeature feature,
			int position, Object oldValue, Object newValue) {
		fireTargetModified(target, feature, position, oldValue, newValue);
	}

	public void targetMultipleModified(EObject target,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {
		fireTargetModified(target, features, positions, oldValues, newValues);
	}

	public void targetRemoved(EObject target) {
		fireTargetRemoved(target);
	}
}
