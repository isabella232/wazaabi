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
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.events.EventHandlerDetailsForm;

public class EventHandlerCellEditor extends AbstractEventHandlerCellEditor {

	private EventHandlerDetailsForm eventHandlerDetailsDescriptor;

	public EventHandlerCellEditor(Composite parent) {
		super(parent);
	}

	@Override
	protected String getHeaderTitle() {
		return "Event Handlers";
	}

	@Override
	protected AbstractDetailsSection getEventHandlerDetailsDescriptor() {
		if (eventHandlerDetailsDescriptor == null)
			eventHandlerDetailsDescriptor = new EventHandlerDetailsForm(
					getMethodLocator());
		return eventHandlerDetailsDescriptor;
	}

	protected MethodLocator getMethodLocator() {
		return null;
	}

}
