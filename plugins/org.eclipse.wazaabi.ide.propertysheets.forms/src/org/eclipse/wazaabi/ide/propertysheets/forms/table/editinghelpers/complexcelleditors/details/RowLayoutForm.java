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

package org.eclipse.wazaabi.ide.propertysheets.forms.table.editinghelpers.complexcelleditors.details;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.complexcelleditors.bindings.IntTextFieldBinding;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class RowLayoutForm extends AbstractDetailsSection {

	private static IntTextFieldBinding INT_TEXT_FIELD_BINDING = new IntTextFieldBinding();

	@Override
	protected Control createSection(Section parent,
			TargetChangeListener targetChangeListener) {
		Text marginBottom = getFormToolkit().createText(parent, "", SWT.BORDER);
		bind(marginBottom, INT_TEXT_FIELD_BINDING,
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__MARGIN_BOTTOM,
				targetChangeListener);
		return marginBottom;
	}

	@Override
	public Object getUniqueID() {
		return SWTStylesPackage.Literals.ROW_LAYOUT_RULE;
	}
}
