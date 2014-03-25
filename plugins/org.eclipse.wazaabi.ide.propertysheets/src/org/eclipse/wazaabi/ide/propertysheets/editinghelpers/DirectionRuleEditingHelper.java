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

import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.EEnumCellEditor;
import org.eclipse.wazaabi.mm.core.CorePackage;
import org.eclipse.wazaabi.mm.core.Direction;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.DirectionRule;

public class DirectionRuleEditingHelper extends AbstractEditingHelper {

	@Override
	public boolean canEdit(Object element) {
		return element instanceof DirectionRule;
	}

	@Override
	public CellEditor getCellEditor(Control control, Object element) {
		return new EEnumCellEditor((Composite) control,
				CorePackage.Literals.DIRECTION);
	}

	@Override
	public Object getValue(Object element) {
		return ((DirectionRule) element).getValue();
	}

	@Override
	public void setValue(Object element, Object value,
			TargetChangeListener listener) {
		Direction oldDirection = ((DirectionRule) element).getValue();
		Direction newDirection = (Direction) ((EEnumLiteral) value)
				.getInstance();
		if (oldDirection != newDirection)
			listener.targetModified((EObject) element,
					CoreStylesPackage.Literals.DIRECTION_RULE__VALUE, -1,
					oldDirection, newDirection);
	}

}
