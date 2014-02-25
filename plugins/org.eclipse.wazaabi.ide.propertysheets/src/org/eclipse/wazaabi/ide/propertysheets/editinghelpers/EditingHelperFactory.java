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

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

public class EditingHelperFactory {

	private final static AbstractEditingHelper DO_NOTHING_HELPER = new AbstractEditingHelper() {
	};

	private final static StringEditingHelper STRING_EDITING_HELPER = new StringEditingHelper();
	private final static BooleanEditingHelper BOOLEAN_EDITING_HELPER = new BooleanEditingHelper();
	private final static ColorEditingHelper COLOR_EDITING_HELPER = new ColorEditingHelper();
	private final static FontEditingHelper FONT_EDITING_HELPER = new FontEditingHelper();

	public AbstractEditingHelper getEditingHelper(StyleRule rule) {
		if (rule.eClass() == CoreStylesPackage.Literals.STRING_RULE)
			return STRING_EDITING_HELPER;
		if (rule.eClass() == CoreStylesPackage.Literals.BOOLEAN_RULE)
			return BOOLEAN_EDITING_HELPER;
		if (rule.eClass() == CoreStylesPackage.Literals.COLOR_RULE)
			return COLOR_EDITING_HELPER;
		if (rule.eClass() == CoreStylesPackage.Literals.FONT_RULE)
			return FONT_EDITING_HELPER;
		return DO_NOTHING_HELPER;
	}
}
