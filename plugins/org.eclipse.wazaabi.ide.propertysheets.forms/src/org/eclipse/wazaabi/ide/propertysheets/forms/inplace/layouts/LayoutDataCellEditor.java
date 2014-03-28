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

package org.eclipse.wazaabi.ide.propertysheets.forms.inplace.layouts;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.forms.inplace.FormBasedPlaceHolderCellEditor;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class LayoutDataCellEditor extends FormBasedPlaceHolderCellEditor {

	public LayoutDataCellEditor(Composite parent) {
		super(parent);
	}

	@Override
	protected String getHeaderTitle() {
		return "Layout Data";
	}

	@Override
	protected String getSelectorSectionTitle() {
		return "Layout Data type:";
	}

	@Override
	protected int getPosition(EObject container, EObject element) {
		return ((StyledElement) container).getStyleRules().indexOf(element);
	}

	@Override
	protected AbstractDescriptorFactory createAbstractDescriptorFactory() {
		return new StyleRuleDescriptorFactory();
	}
}
