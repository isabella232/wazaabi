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

package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.propertysheets.MethodLocator;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.events.PathSelectorDetailsForm;

public class PathSelectorCellEditor extends AbstractEventHandlerCellEditor {

	private PathSelectorDetailsForm eventHandlerDetailsDescriptor;

	public PathSelectorCellEditor(Composite parent) {
		super(parent);
	}

	@Override
	protected String getHeaderTitle() {
		return "Path selector";
	}

	@Override
	protected AbstractDetailsSection getEventHandlerDetailsDescriptor() {
		if (eventHandlerDetailsDescriptor == null)
			eventHandlerDetailsDescriptor = new PathSelectorDetailsForm();
		return eventHandlerDetailsDescriptor;
	}

	protected MethodLocator getMethodLocator() {
		return null;
	}

}
