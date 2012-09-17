/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.propertysheets.eventhandlers;

import org.eclipse.wazaabi.engine.edp.adapters.BindingAdapter;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class RefreshDefaultBindingAction extends AbstractBindingAction {

	public void execute(AbstractComponent uiComponent,
			EventHandler eventHandler, Event event) {
		Widget domainWidget = getInputWidget(uiComponent);
		if (domainWidget == null)
			return;

		// get the text filled by the user in the property page
		String textfieldValue = ((TextComponent) uiComponent).getText();

		Binding domainToWidgetCurrentBinding = getExistingDefaultBinding(
				domainWidget, BindingDirection.DOMAIN_TO_WIDGET);
		// Binding widgetToDomainCurrentBinding = getExistingDefaultBinding(
		// domainWidget, BindingDirection.WIDGET_TO_DOMAIN);

		String bindingDomainBoundProperty = null;

		if (domainToWidgetCurrentBinding != null)
			bindingDomainBoundProperty = getStringParameterValue(
					domainToWidgetCurrentBinding,
					BindingAdapter.SOURCE_PARAMETER_NAME);

		if (bindingDomainBoundProperty == null
				|| "".equals(bindingDomainBoundProperty)) { //$NON-NLS-1$
			if (textfieldValue != null && !"".equals(textfieldValue)) //$NON-NLS-1$
				((TextComponent) uiComponent).setText(null);
		} else {
			if (!bindingDomainBoundProperty.equals(textfieldValue)) //$NON-NLS-1$
				((TextComponent) uiComponent)
						.setText(bindingDomainBoundProperty);
		}
	}
}
