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
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gef.EditDomain;
import org.eclipse.wazaabi.ide.ui.editparts.commands.AbstractSetUniqueStyleRuleCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.SetUniqueStringRuleCommand;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public abstract class UpdateStyleRuleAction extends AbstractStyleRuleAction {

	private static final String COMMAND_PACKAGE_NAME = "org.eclipse.wazaabi.ide.ui.editparts.commands"; //$NON-NLS-1$

	protected String getCommandPackageName() {
		return COMMAND_PACKAGE_NAME;
	}

	public void execute(AbstractComponent uiComponent,
			EventHandler eventHandler, Event event) {

		TransactionalEditingDomain transactionaEditingDomain = (TransactionalEditingDomain) uiComponent
				.get("TransactionalEditingDomain"); //$NON-NLS-1$
		if (transactionaEditingDomain == null)
			return;

		EditDomain editDomain = (EditDomain) uiComponent.get("EditDomain"); //$NON-NLS-1$
		if (editDomain == null)
			return;

		String propertyName = getStringParameterValue(eventHandler,
				"propertyName"); //$NON-NLS-1$
		if (propertyName == null || "".equals(propertyName)) //$NON-NLS-1$
			return;

		Widget domainWidget = getInputWidget(uiComponent);
		if (domainWidget == null)
			return;

		Object domainValue = getFirstStyleRuleValue(domainWidget,
				getStringParameterValue(eventHandler, "propertyName")); //$NON-NLS-1$

		EAttribute uiEAttribute = resolveEAttribute(uiComponent, eventHandler,
				getStringParameterValue(eventHandler, "source")); //$NON-NLS-1$

		if (uiEAttribute != null) {
			Object uiValue = uiComponent.eGet(uiEAttribute);
			if (areEquals(uiValue, domainValue))
				return;
			String commandClassName = getCommandPackageName() + '.'
					+ getStringParameterValue(eventHandler, "command"); // $NON-NLS-1$
			AbstractSetUniqueStyleRuleCommand command = createSetUniqueStyleRuleCommand(
					commandClassName, domainWidget, propertyName, uiValue,
					transactionaEditingDomain);
			if (command != null)
				editDomain.getCommandStack().execute(command);

		}
	}

	protected AbstractSetUniqueStyleRuleCommand createSetUniqueStyleRuleCommand(
			String commandClassName, Widget widget, String propertyName,
			Object newValue,
			TransactionalEditingDomain transactionalEditingDomain) {
		try {
			Class<?> clazz = getClass().getClassLoader().loadClass(
					commandClassName);
			if (clazz != null
					&& AbstractSetUniqueStyleRuleCommand.class
							.isAssignableFrom(clazz)) {
				AbstractSetUniqueStyleRuleCommand command = (SetUniqueStringRuleCommand) clazz
						.newInstance();
				command.setTransactionalEditingDomain(transactionalEditingDomain);
				command.setPropertyName(propertyName);
				command.setWidget(widget);
				command.setNewValue(newValue);
				return command;
			}
		} catch (ClassNotFoundException e) {
			e.printStackTrace(); // TODO : log this
		} catch (InstantiationException e) {
			e.printStackTrace(); // TODO : log this
		} catch (IllegalAccessException e) {
			e.printStackTrace(); // TODO : log this
		}
		return null;
	}
}
