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

package org.eclipse.wazaabi.ide.propertysheets.forms.editinghelpers;

import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.AbstractEditingHelper;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.EditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptor.PlaceHolderRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

public class FormBasedEditingHelperFactory extends EditingHelperFactory {

	private final static LayoutEditingHelper LAYOUT_EDITING_HELPER = new LayoutEditingHelper();
	private final static LayoutDataEditingHelper LAYOUT_DATA_EDITING_HELPER = new LayoutDataEditingHelper();

	public AbstractEditingHelper getEditingHelper(StyleRule rule) {
		if (rule instanceof PlaceHolderRule) {
			if (ContainerEditPart.LAYOUT_PROPERTY_NAME
					.equals(((PlaceHolderRule) rule).getPropertyName()))
				return LAYOUT_EDITING_HELPER;
			if (AbstractComponentEditPart.LAYOUT_DATA_PROPERTY_NAME
					.equals(((PlaceHolderRule) rule).getPropertyName()))
				return LAYOUT_DATA_EDITING_HELPER;
		}
		if (rule instanceof LayoutRule)
			return LAYOUT_EDITING_HELPER;
		if (rule instanceof LayoutDataRule)
			return LAYOUT_DATA_EDITING_HELPER;
		return super.getEditingHelper(rule);
	}
}
