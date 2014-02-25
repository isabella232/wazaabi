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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StringRule;

public class StringEditingHelper extends AbstractEditingHelper {
	@Override
	public CellEditor getCellEditor(Control control, Object element) {
		return new TextCellEditor((Composite) control);
	}

	@Override
	public Object getValue(Object element) {
		return ((StringRule) element).getValue();
	}

	@Override
	public void setValue(Object element, Object value,
			TargetChangeListener listener) {
		listener.targetModified((EObject) element,
				CoreStylesPackage.Literals.STRING_RULE__VALUE, -1,
				((StringRule) element).getValue(), (String) value);
	}
}
