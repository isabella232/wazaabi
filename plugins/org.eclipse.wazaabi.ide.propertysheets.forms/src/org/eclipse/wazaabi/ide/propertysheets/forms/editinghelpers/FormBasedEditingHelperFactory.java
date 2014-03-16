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

	private LayoutEditingHelper layoutEditingHelper = null;
	private LayoutDataEditingHelper layoutDataEditingHelper = null;
	private EventHandlerEditingHelper eventHandlerEditingHelper = null;

	protected EventHandlerEditingHelper createEventHandlerEditingHelper() {
		return new EventHandlerEditingHelper(null);
	}

	protected LayoutDataEditingHelper createLayoutDataEditingHelper() {
		return new LayoutDataEditingHelper();
	}

	protected LayoutEditingHelper createLayoutEditingHelper() {
		return new LayoutEditingHelper();
	}

	@Override
	public AbstractEditingHelper getEditingHelper(EObject row) {
		if (row instanceof PlaceHolderRule) {
			if (ContainerEditPart.LAYOUT_PROPERTY_NAME
					.equals(((PlaceHolderRule) row).getPropertyName()))
				return getLayoutEditingHelper();
			if (AbstractComponentEditPart.LAYOUT_DATA_PROPERTY_NAME
					.equals(((PlaceHolderRule) row).getPropertyName()))
				return getLayoutDataEditingHelper();
		}
		if (row instanceof LayoutRule)
			return getLayoutEditingHelper();
		if (row instanceof LayoutDataRule)
			return getLayoutDataEditingHelper();
		if (row.eClass() == EDPHandlersPackage.Literals.EVENT_HANDLER)
			return getEventHandlerEditingHelper();
		return super.getEditingHelper(row);
	}

	protected final EventHandlerEditingHelper getEventHandlerEditingHelper() {
		if (eventHandlerEditingHelper == null)
			eventHandlerEditingHelper = createEventHandlerEditingHelper();
		return eventHandlerEditingHelper;
	}

	protected final LayoutDataEditingHelper getLayoutDataEditingHelper() {
		if (layoutDataEditingHelper == null)
			layoutDataEditingHelper = createLayoutDataEditingHelper();
		return layoutDataEditingHelper;
	}

	protected final LayoutEditingHelper getLayoutEditingHelper() {
		if (layoutEditingHelper == null)
			layoutEditingHelper = createLayoutEditingHelper();
		return layoutEditingHelper;
	}
}
