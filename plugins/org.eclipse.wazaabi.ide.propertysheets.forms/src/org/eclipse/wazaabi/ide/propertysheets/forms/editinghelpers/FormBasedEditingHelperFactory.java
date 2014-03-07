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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptor.PlaceHolderRule;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.AbstractEditingHelper;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.EditingHelperFactory;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

public class FormBasedEditingHelperFactory extends EditingHelperFactory {

	private final static LayoutEditingHelper LAYOUT_EDITING_HELPER = new LayoutEditingHelper();
	private final static LayoutDataEditingHelper LAYOUT_DATA_EDITING_HELPER = new LayoutDataEditingHelper();
	private final static EventHandlerEditingHelper EVENT_HANDLER_EDITING_HELPER = new EventHandlerEditingHelper();

	@Override
	public AbstractEditingHelper getEditingHelper(EObject row) {
		if (row instanceof PlaceHolderRule) {
			if (ContainerEditPart.LAYOUT_PROPERTY_NAME
					.equals(((PlaceHolderRule) row).getPropertyName()))
				return LAYOUT_EDITING_HELPER;
			if (AbstractComponentEditPart.LAYOUT_DATA_PROPERTY_NAME
					.equals(((PlaceHolderRule) row).getPropertyName()))
				return LAYOUT_DATA_EDITING_HELPER;
		}
		if (row instanceof LayoutRule)
			return LAYOUT_EDITING_HELPER;
		if (row instanceof LayoutDataRule)
			return LAYOUT_DATA_EDITING_HELPER;
		if (row.eClass() == EDPHandlersPackage.Literals.EVENT_HANDLER)
			return EVENT_HANDLER_EDITING_HELPER;
		return super.getEditingHelper(row);
	}
}
