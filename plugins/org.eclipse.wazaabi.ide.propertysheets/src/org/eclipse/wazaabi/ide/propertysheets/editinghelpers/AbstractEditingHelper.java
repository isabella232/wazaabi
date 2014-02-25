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

package org.eclipse.wazaabi.ide.propertysheets.editinghelpers;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;

public abstract class AbstractEditingHelper {

	public boolean canEdit(Object element) {
		return true;
	}

	public CellEditor getCellEditor(Control control, Object element) {
		return null;
	}

	public Object getValue(Object element) {
		return null;
	}

	public void setValue(Object element, Object value, TargetChangeListener listener) {
	}

}
