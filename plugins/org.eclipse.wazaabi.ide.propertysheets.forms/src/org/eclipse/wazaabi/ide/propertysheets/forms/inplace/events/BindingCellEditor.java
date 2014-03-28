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

package org.eclipse.wazaabi.ide.propertysheets.forms.inplace.events;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.propertysheets.forms.inplace.AbstractDetailsSection;

public class BindingCellEditor extends AbstractEventHandlerCellEditor {

	private BindingDetailsForm eventHandlerDetailsDescriptor;

	public BindingCellEditor(Composite parent) {
		super(parent);
	}

	@Override
	protected String getHeaderTitle() {
		return "Binding";
	}

	@Override
	protected AbstractDetailsSection getEventHandlerDetailsDescriptor() {
		if (eventHandlerDetailsDescriptor == null)
			eventHandlerDetailsDescriptor = new BindingDetailsForm();
		return eventHandlerDetailsDescriptor;
	}
}
