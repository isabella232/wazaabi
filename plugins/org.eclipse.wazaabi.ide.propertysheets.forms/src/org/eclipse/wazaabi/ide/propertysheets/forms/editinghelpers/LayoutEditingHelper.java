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
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.AbstractEditingHelper;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.LayoutCellEditor;

public class LayoutEditingHelper extends AbstractEditingHelper {

	@Override
	public boolean canEdit(Object element) {
		return false;
	}

	@Override
	public CellEditor getCellEditor(Control control, Object element) {
		return new LayoutCellEditor((Composite) control);
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
