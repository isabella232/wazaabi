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
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

public class EditingHelperFactory {

	private final static AbstractEditingHelper DO_NOTHING_HELPER = new AbstractEditingHelper() {
	};

	private final static StringEditingHelper STRING_EDITING_HELPER = new StringEditingHelper();
	private final static BooleanEditingHelper BOOLEAN_EDITING_HELPER = new BooleanEditingHelper();
	private final static ColorEditingHelper COLOR_EDITING_HELPER = new ColorEditingHelper();
	private final static FontEditingHelper FONT_EDITING_HELPER = new FontEditingHelper();
	private final static DirectionRuleEditingHelper DIRECTION_RULE_EDITING_HELPER = new DirectionRuleEditingHelper();

	public AbstractEditingHelper getEditingHelper(EObject row) {
		if (row.eClass() == CoreStylesPackage.Literals.STRING_RULE)
			return STRING_EDITING_HELPER;
		if (row.eClass() == CoreStylesPackage.Literals.BOOLEAN_RULE)
			return BOOLEAN_EDITING_HELPER;
		if (row.eClass() == CoreStylesPackage.Literals.COLOR_RULE)
			return COLOR_EDITING_HELPER;
		if (row.eClass() == CoreStylesPackage.Literals.FONT_RULE)
			return FONT_EDITING_HELPER;
		if (row.eClass() == CoreStylesPackage.Literals.DIRECTION_RULE)
			return DIRECTION_RULE_EDITING_HELPER;
		return DO_NOTHING_HELPER;
	}
}
