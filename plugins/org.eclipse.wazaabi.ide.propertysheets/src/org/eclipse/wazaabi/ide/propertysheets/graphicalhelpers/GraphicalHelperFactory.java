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
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptor.PlaceHolderRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class GraphicalHelperFactory {

	private final static AbstractGraphicalHelper DO_NOTHING_HELPER = new AbstractGraphicalHelper() {
	};

	private final static StringGraphicalHelper STRING_GRAPHICAL_HELPER = new StringGraphicalHelper();
	private final static BooleanGraphicalHelper BOOLEAN_GRAPHICAL_HELPER = new BooleanGraphicalHelper();
	private final static ColorGraphicalHelper COLOR_GRAPHICAL_HELPER = new ColorGraphicalHelper();
	private final static FontGraphicalHelper FONT_GRAPHICAL_HELPER = new FontGraphicalHelper();
	private final static LayoutGraphicalHelper LAYOUT_GRAPHICAL_HELPER = new LayoutGraphicalHelper();
	private final static LayoutDataGraphicalHelper LAYOUT_DATA_GRAPHICAL_HELPER = new LayoutDataGraphicalHelper();
	private final static EventHandlerGraphicalHelper EVENT_HANDER_GRAPHICAL_HELPER = new EventHandlerGraphicalHelper();
	private final static BindingGraphicalHelper BINDING_GRAPHICAL_HELPER = new BindingGraphicalHelper();

	public AbstractGraphicalHelper getGraphicalHelper(EObject row) {
		if (row.eClass() == CoreStylesPackage.Literals.STRING_RULE)
			return STRING_GRAPHICAL_HELPER;
		if (row.eClass() == CoreStylesPackage.Literals.BOOLEAN_RULE)
			return BOOLEAN_GRAPHICAL_HELPER;
		if (row.eClass() == CoreStylesPackage.Literals.COLOR_RULE)
			return COLOR_GRAPHICAL_HELPER;
		if (row.eClass() == CoreStylesPackage.Literals.FONT_RULE)
			return FONT_GRAPHICAL_HELPER;
		if (row.eClass() == CoreStylesPackage.Literals.STACK_LAYOUT_RULE
				|| row.eClass() == CoreStylesPackage.Literals.TABBED_LAYOUT_RULE
				|| row.eClass() == SWTStylesPackage.Literals.ROW_LAYOUT_RULE
				|| row.eClass() == CoreStylesPackage.Literals.BOX_LAYOUT_RULE
				|| row.eClass() == SWTStylesPackage.Literals.GRID_LAYOUT_RULE
				|| row.eClass() == SWTStylesPackage.Literals.FILL_LAYOUT_RULE)
			return LAYOUT_GRAPHICAL_HELPER;
		if (row.eClass() == SWTStylesPackage.Literals.ROW_DATA_RULE
				|| row.eClass() == SWTStylesPackage.Literals.GRID_DATA_RULE)
			return LAYOUT_DATA_GRAPHICAL_HELPER;
		if (row.eClass() == EDPHandlersPackage.Literals.BINDING)
			return BINDING_GRAPHICAL_HELPER;
		if (row.eClass() == EDPHandlersPackage.Literals.EVENT_HANDLER)
			return EVENT_HANDER_GRAPHICAL_HELPER;
		if (row instanceof PlaceHolderRule) {
			if (ContainerEditPart.LAYOUT_PROPERTY_NAME
					.equals(((PlaceHolderRule) row).getPropertyName()))
				return LAYOUT_GRAPHICAL_HELPER;
			if (ContainerEditPart.LAYOUT_DATA_PROPERTY_NAME
					.equals(((PlaceHolderRule) row).getPropertyName()))
				return LAYOUT_DATA_GRAPHICAL_HELPER;
		}

		return DO_NOTHING_HELPER;
	}
}
