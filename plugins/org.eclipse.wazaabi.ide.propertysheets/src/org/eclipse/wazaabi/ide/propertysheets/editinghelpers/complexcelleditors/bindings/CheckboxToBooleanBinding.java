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

package org.eclipse.wazaabi.ide.propertysheets.editinghelpers.complexcelleditors.bindings;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;

public class CheckboxToBooleanBinding extends AbstractBinding {

	@Override
	protected Object convertToExpectedValue(Object value) {
		if (value instanceof Boolean)
			return (Boolean) value;
		return null;
	}

	@Override
	protected String getErrorMessage(Object value) {
		return null;
	}

	@Override
	public void refresh(Control control) {
		((Button) control).setSelection(((Boolean) getDomainValue(control))
				.booleanValue());
	}

	@Override
	protected void addListeners(final Control control) {
		((Button) control).addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				Boolean newBooleanValue = ((Button) control).getSelection();
				Object domainValue = getDomainValue(control);
				if (newBooleanValue.equals(domainValue))
					return;
				TargetChangeListener listener = getTargetChangeListener(control);
				if (listener != null)
					listener.targetModified(getDomainObject(control),
							getEStructuralFeature(control), -1, domainValue,
							newBooleanValue);
			}
		});
	}

}
