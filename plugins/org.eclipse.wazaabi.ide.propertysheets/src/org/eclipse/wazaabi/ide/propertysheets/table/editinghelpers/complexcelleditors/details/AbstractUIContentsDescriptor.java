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

package org.eclipse.wazaabi.ide.propertysheets.table.editinghelpers.complexcelleditors.details;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.complexcelleditors.bindings.AbstractBinding;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;

public abstract class AbstractUIContentsDescriptor {

	protected static final String BINDING_KEY = "BindingKey"; //$NON-NLS-1$

	protected final void bind(Control control, AbstractBinding binding,
			EStructuralFeature feature,
			TargetChangeListener targetChangeListener) {
		assert binding != null;
		assert control != null && !control.isDisposed();
		binding.bind(control, feature, targetChangeListener);
		control.setData(BINDING_KEY, binding);
	}

	abstract public Control createContents(Control parent,
			TargetChangeListener targetChangeListener);

	protected AbstractBinding getBinding(Control control) {
		if (control != null && !control.isDisposed())
			return (AbstractBinding) control.getData(BINDING_KEY);
		return null;
	}

	abstract public Object getUniqueID();

	public void refresh(Control control) {
		if (control instanceof Composite)
			for (Control child : ((Composite) control).getChildren())
				refresh(child);
		AbstractBinding binding = getBinding(control);
		if (binding != null)
			binding.refresh(control);
	}

	public void setInput(Control control, EObject newInput) {
		if (control instanceof Composite)
			for (Control child : ((Composite) control).getChildren())
				setInput(child, newInput);
		AbstractBinding binding = getBinding(control);
		if (binding != null)
			binding.setInput(control, newInput);
	}

	abstract public String getTitle();

}
