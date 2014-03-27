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
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;

public class TextToStringBinding extends AbstractBinding {

	@Override
	protected void addListeners(final Control control) {

		control.addFocusListener(new FocusAdapter() {

			@Override
			public void focusLost(FocusEvent e) {
				applyChanges(control);
			}
		});
		control.addKeyListener(new KeyAdapter() {

			@Override
			public void keyPressed(KeyEvent e) {
				if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
					e.doit = false;
					applyChanges(control);
				} else if ((e.stateMask & SWT.CTRL) == SWT.CTRL) {
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

	@Override
	protected Object convertToExpectedValue(Object value) {
		if ("".equals(value)) //$NON-NLS-1$
			return null;
		return value;
	}

	@Override
	public void refresh(Control control) {
		String value = (String) getDomainValue(control);
		value = (value != null ? value : ""); //$NON-NLS-1$
		if (!((Text) control).getText().equals(value))
			((Text) control).setText(value);
	}

	protected void applyChanges(Control control) {
		String newStringValue = ((Text) control).getText();

		String errorMessage = getErrorMessage(newStringValue);
		Object domainValue = getDomainValue(control);

		if (errorMessage == null) {
			Object newValue = convertToExpectedValue(newStringValue);
			if (newValue == null) {
				if (domainValue == null)
					return;
			} else if (newValue.equals(domainValue))
				return;
			TargetChangeListener listener = getTargetChangeListener(control);
			if (listener != null)
				listener.targetModified(getDomainObject(control),
						getEStructuralFeature(control), -1, domainValue,
						newValue);
		} else
			; // TODO
	}
}
