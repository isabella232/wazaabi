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
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

public class BooleanEditingHelper extends AbstractEditingHelper {

	@Override
	public boolean canEdit(Object element) {
		return element instanceof BooleanRule;
	}

	@Override
	public CellEditor getCellEditor(Control control, Object element) {
		return new CheckboxCellEditor((Composite) control);
	}

	@Override
	public Object getValue(Object element) {
		return ((BooleanRule) element).isValue();
	}

	@Override
	public void setValue(Object element, Object value,
			TargetChangeListener listener) {
		listener.targetModified((EObject) element,
				CoreStylesPackage.Literals.BOOLEAN_RULE__VALUE, -1,
				((BooleanRule) element).isValue(), (Boolean) value);
	}

}
