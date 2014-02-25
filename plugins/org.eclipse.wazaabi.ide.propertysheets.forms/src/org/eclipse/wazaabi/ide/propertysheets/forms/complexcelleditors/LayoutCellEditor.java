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

public class LayoutCellEditor extends FormBasedPlaceHolderRuleCellEditor {

	public LayoutCellEditor(Composite parent) {
		super(parent);
	}

	@Override
	protected String getHeaderTitle() {
		return "Layout";
	}

}
