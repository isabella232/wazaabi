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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public abstract class RefreshStyleRuleAction extends AbstractStyleRuleAction {

	public void execute(AbstractComponent uiComponent,
			EventHandler eventHandler, Event event) {
		// we get the EAttribute of the ui component to check.
		EAttribute uiEAttribute = resolveEAttribute(uiComponent, eventHandler,
				getStringParameterValue(eventHandler, "target") //$NON-NLS-1$
		);
		if (uiEAttribute == null)
			return;
		Object uiValue = uiComponent.eGet(uiEAttribute);
		// TODO : what about unsettable eAttributes ?

		// We get the style rule's value to display
		Object domainValue = getFirstStyleRuleValue(uiComponent, eventHandler);
		if (!areEquals(uiValue, domainValue))
			setUIEAttribute(uiComponent, uiEAttribute, domainValue);
	}

	protected abstract void setUIEAttribute(AbstractComponent uiComponent,
			EAttribute uiEAttribute, Object newValue);

	/**
	 * Fetches a domain widget from uiComponent's context (the entry key is
	 * 'input'), then, using the value of the string parameter whose name is
	 * 'propertyName' as style rule's property name, returns the rule's value.
	 * 
	 * @param uiComponent
	 * @param eventHandler
	 * @return
	 */
	protected Object getFirstStyleRuleValue(AbstractComponent uiComponent,
			EventHandler eventHandler) {
		Widget domainWidget = getInputWidget(uiComponent);
		if (domainWidget != null)
			return getFirstStyleRuleValue(domainWidget,
					getStringParameterValue(eventHandler, "propertyName")); //$NON-NLS-1$
		return AbstractStyleRuleAction.NO_STYLE_RULE;
	}

}
