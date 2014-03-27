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
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeService;
import org.eclipse.wazaabi.ide.propertysheets.tabbed.ImageUtils;

public abstract class InPlaceCellEditor extends CellEditor implements
		TargetChangeService, TargetChangeListener {

	private List<TargetChangeListener> listeners = new ArrayList<TargetChangeListener>();
	private Object input = null;
	private ImageDescriptor closeImageDescriptor = null;

	protected class CloseAction extends Action {

		public CloseAction() {
			super();
		}

		public CloseAction(String text) {
			super(text);
		}

		public CloseAction(String text, ImageDescriptor image) {
			super(text, image);
		}

		public CloseAction(String text, int style) {
			super(text, style);
		}

		@Override
		public void run() {
			if (getControl() != null && !getControl().isDisposed())
				getControl().dispose();
		}

	};

	public InPlaceCellEditor(Composite parent) {
		super(parent);
	}

	protected CloseAction createCloseAction() {
		if (closeImageDescriptor == null)
			closeImageDescriptor = ImageUtils.getImageDescriptor(
					"icons/delete.gif", PlaceHolderRuleCellEditor.class); //$NON-NLS-1$

		CloseAction closeAction = new CloseAction(null, closeImageDescriptor);
		// closeAction.setImageDescriptor(closeImageDescriptor);
		return closeAction;
	}

	public void addTargetChangeListener(TargetChangeListener listener) {
		listeners.add(listener);
	}

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

	protected void fireTargetAdded(EObject container, EObject target,
			int position) {
		for (TargetChangeListener listener : listeners)
			listener.targetAdded(container, target, position);
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

	protected void fireTargetRemoved(EObject container, EObject target) {
		for (TargetChangeListener listener : listeners)
			listener.targetRemoved(container, target);
	}

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

	public void targetAdded(EObject container, EObject target, int position) {
		fireTargetAdded(container, target, position);
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

	public void targetRemoved(EObject container, EObject target) {
		fireTargetRemoved(container, target);
	}

	protected void fireRedo() {
		for (TargetChangeListener listener : listeners)
			listener.redo();
	}

	protected void fireUndo() {
		for (TargetChangeListener listener : listeners)
			listener.undo();
	}

	public void undo() {
		fireUndo();
	}

	public void redo() {
		fireRedo();
	}

}
