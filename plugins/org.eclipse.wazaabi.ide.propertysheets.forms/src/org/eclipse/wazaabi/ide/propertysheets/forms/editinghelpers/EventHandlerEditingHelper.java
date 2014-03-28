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

package org.eclipse.wazaabi.ide.propertysheets.forms.editinghelpers;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.MethodLocator;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.AbstractEditingHelper;
import org.eclipse.wazaabi.ide.propertysheets.forms.inplace.events.EventHandlerCellEditor;

public class EventHandlerEditingHelper extends AbstractEditingHelper {

	private final MethodLocator methodLocator;

	public MethodLocator getMethodLocator() {
		return methodLocator;
	}

	public EventHandlerEditingHelper(MethodLocator methodLocator) {
		this.methodLocator = methodLocator;
	}

	@Override
	public boolean canEdit(Object element) {
		return false;
	}

	@Override
	public CellEditor getCellEditor(Control control, Object element) {
		return new EventHandlerCellEditor((Composite) control) {

			@Override
			protected MethodLocator getMethodLocator() {
				return EventHandlerEditingHelper.this.getMethodLocator();
			}
		};
	}

	@Override
	public Object getValue(Object element) {
		return element;
	}

	@Override
	public void setValue(Object element, Object value,
			TargetChangeListener listener) {
	}

}
