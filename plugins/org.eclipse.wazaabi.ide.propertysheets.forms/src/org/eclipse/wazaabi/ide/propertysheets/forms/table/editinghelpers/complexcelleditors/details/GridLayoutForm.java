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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class GridLayoutForm extends AbstractDetailsSection {

	@Override
	protected Control createSection(Section parent,
			TargetChangeListener targetChangeListener) {
		Button button = new Button(parent, SWT.PUSH);
		button.setText("Grid");
		return button;
	}

	@Override
	public Object getUniqueID() {
		return SWTStylesPackage.Literals.GRID_LAYOUT_RULE;
	}

}
