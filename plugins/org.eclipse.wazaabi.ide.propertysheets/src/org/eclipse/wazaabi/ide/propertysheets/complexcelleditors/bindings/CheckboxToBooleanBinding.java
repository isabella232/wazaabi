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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;

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
		boolean newSelection = ((Boolean) getDomainValue(control))
				.booleanValue();
		if (((Button) control).getSelection() != newSelection)
			((Button) control).setSelection(newSelection);
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

		control.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if ((e.stateMask & SWT.CTRL) == SWT.CTRL) {
					if (e.keyCode == 'Z' || e.keyCode == 'z') {
						TargetChangeListener listener = getTargetChangeListener(control);
						if (listener != null)
							listener.undo();
					} else if (e.keyCode == 'Y' || e.keyCode == 'y') {
						TargetChangeListener listener = getTargetChangeListener(control);
						if (listener != null)
							listener.redo();
					}
				}
			}
		});

	}

}
