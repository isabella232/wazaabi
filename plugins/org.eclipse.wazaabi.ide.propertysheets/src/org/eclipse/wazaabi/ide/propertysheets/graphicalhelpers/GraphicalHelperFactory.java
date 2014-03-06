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

package org.eclipse.wazaabi.ide.propertysheets.graphicalhelpers;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptor.PlaceHolderRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class GraphicalHelperFactory {

	private final static AbstractGraphicalHelper DO_NOTHING_HELPER = new AbstractGraphicalHelper() {
	};

	private final static StringGraphicalHelper STRING_GRAPHICAL_HELPER = new StringGraphicalHelper();
	private final static BooleanGraphicalHelper BOOLEAN_GRAPHICAL_HELPER = new BooleanGraphicalHelper();
	private final static ColorGraphicalHelper COLOR_GRAPHICAL_HELPER = new ColorGraphicalHelper();
	private final static FontGraphicalHelper FONT_GRAPHICAL_HELPER = new FontGraphicalHelper();
	private final static LayoutGraphicalHelper LAYOUT_GRAPHICAL_HELPER = new LayoutGraphicalHelper();

	public AbstractGraphicalHelper getGraphicalHelper(EObject rule) {
		if (rule.eClass() == CoreStylesPackage.Literals.STRING_RULE)
			return STRING_GRAPHICAL_HELPER;
		if (rule.eClass() == CoreStylesPackage.Literals.BOOLEAN_RULE)
			return BOOLEAN_GRAPHICAL_HELPER;
		if (rule.eClass() == CoreStylesPackage.Literals.COLOR_RULE)
			return COLOR_GRAPHICAL_HELPER;
		if (rule.eClass() == CoreStylesPackage.Literals.FONT_RULE)
			return FONT_GRAPHICAL_HELPER;
		if (rule.eClass() == CoreStylesPackage.Literals.STACK_LAYOUT_RULE
				|| rule.eClass() == CoreStylesPackage.Literals.TABBED_LAYOUT_RULE
				|| rule.eClass() == SWTStylesPackage.Literals.ROW_LAYOUT_RULE
				|| rule.eClass() == SWTStylesPackage.Literals.GRID_LAYOUT_RULE
				|| rule.eClass() == SWTStylesPackage.Literals.FILL_LAYOUT_RULE)
			return LAYOUT_GRAPHICAL_HELPER;
		if (rule instanceof PlaceHolderRule) {
			if (ContainerEditPart.LAYOUT_PROPERTY_NAME
					.equals(((PlaceHolderRule) rule).getPropertyName()))
				return LAYOUT_GRAPHICAL_HELPER;
		}
		return DO_NOTHING_HELPER;
	}
}
