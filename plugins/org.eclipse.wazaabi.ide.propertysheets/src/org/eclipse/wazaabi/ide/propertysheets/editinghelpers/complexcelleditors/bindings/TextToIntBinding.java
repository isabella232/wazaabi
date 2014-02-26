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

import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

public class TextToIntBinding extends TextToStringdBinding {

	@Override
	protected Object convertToExpectedValue(Object value) {
		try {
			return Integer.parseInt((String) value);
		} catch (NumberFormatException e) {

		}
		return null;
	}

	@Override
	protected String getErrorMessage(Object value) {
		try {
			Integer.parseInt((String) value);
			return null;
		} catch (NumberFormatException e) {
		}
		return "Integer expected";
	}

	@Override
	public void refresh(Control control) {
		((Text) control).setText(Integer
				.toString((Integer) getDomainValue(control)));
	}

}
