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
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;

public class CollectionLookAndFeelRuleRuleEditingHelper extends
		AbstractEditingHelper {

	@Override
	public boolean canEdit(Object element) {
		return element instanceof LookAndFeelRule;
	}

	@Override
	public CellEditor getCellEditor(Control control, Object element) {
		return new EEnumCellEditor((Composite) control,
				CoreCollectionsStylesPackage.Literals.LOOK_AND_FEEL);
	}

	@Override
	public Object getValue(Object element) {
		return ((LookAndFeelRule) element).getValue();
	}

	@Override
	public void setValue(Object element, Object value,
			TargetChangeListener listener) {
		LookAndFeel oldLookAndFeel = ((LookAndFeelRule) element).getValue();
		LookAndFeel newLookAndFeel = (LookAndFeel) ((EEnumLiteral) value)
				.getInstance();
		if (oldLookAndFeel != newLookAndFeel)
			listener.targetModified(
					(EObject) element,
					CoreCollectionsStylesPackage.Literals.LOOK_AND_FEEL_RULE__VALUE,
					-1, oldLookAndFeel, newLookAndFeel);
	}

}
