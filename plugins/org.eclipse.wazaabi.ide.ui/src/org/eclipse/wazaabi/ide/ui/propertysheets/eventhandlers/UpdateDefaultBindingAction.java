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

import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gef.EditDomain;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.InsertNewBindingCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.RemoveBindingCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.ReplaceBindingCommand;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class UpdateDefaultBindingAction extends AbstractBindingAction {

	public void execute(AbstractComponent uiComponent,
			EventHandler eventHandler, Event event) {
		TransactionalEditingDomain transactionaEditingDomain = (TransactionalEditingDomain) uiComponent
				.get("TransactionalEditingDomain"); //$NON-NLS-1$
		if (transactionaEditingDomain == null)
			return;

		EditDomain editDomain = (EditDomain) uiComponent.get("EditDomain"); //$NON-NLS-1$
		if (editDomain == null)
			return;

		Widget domainWidget = getInputWidget(uiComponent);
		if (domainWidget == null)
			return;

		// find the default bindings associated to this widget if they exist
		Command widgetToDomainBindingCommand = null;
		Command domainToWidgetBindingCommand = null;

		// get the text filled by the user in the property page
		String textfieldValue = ((TextComponent) uiComponent).getText();

		Binding domainToWidgetCurrentBinding = getExistingDefaultBinding(
				domainWidget, BindingDirection.DOMAIN_TO_WIDGET);
		Binding widgetToDomainCurrentBinding = getExistingDefaultBinding(
				domainWidget, BindingDirection.WIDGET_TO_DOMAIN);

		if (textfieldValue == null || "".equals(textfieldValue)) { //$NON-NLS-1$
			if (domainToWidgetCurrentBinding == null)
				return;
			else {
				domainToWidgetBindingCommand = new RemoveBindingCommand();
				((RemoveBindingCommand) domainToWidgetBindingCommand)
						.setEventDispatcher(domainWidget);
				((RemoveBindingCommand) domainToWidgetBindingCommand)
						.setBinding(domainToWidgetCurrentBinding);
				// TODO : localize it
				domainToWidgetBindingCommand.setLabel("Remove Binding"); //$NON-NLS-1$ 
			}
		} else {
			if (domainToWidgetCurrentBinding == null) {
				domainToWidgetCurrentBinding = createDefaultBindingFor(
						domainWidget.eClass(), textfieldValue,
						BindingDirection.DOMAIN_TO_WIDGET);
				if (domainToWidgetCurrentBinding != null) {
					domainToWidgetBindingCommand = new InsertNewBindingCommand();
					((InsertNewBindingCommand) domainToWidgetBindingCommand)
							.setEventDispatcher(domainWidget);
					((InsertNewBindingCommand) domainToWidgetBindingCommand)
							.setBinding(domainToWidgetCurrentBinding);
					// TODO : localize it
					domainToWidgetBindingCommand.setLabel("Add Binding"); //$NON-NLS-1$ 
				}
			} else {
				if (isSameBinding(
						domainWidget.eClass(),
						textfieldValue,
						getEventDispatcherDefaultPropertyBoundFor(domainWidget.eClass(),
								BindingDirection.DOMAIN_TO_WIDGET), null,
						domainToWidgetCurrentBinding,
						BindingDirection.DOMAIN_TO_WIDGET))
					return;
				else {
					domainToWidgetBindingCommand = new ReplaceBindingCommand();
					((ReplaceBindingCommand) domainToWidgetBindingCommand)
							.setEventDispatcher(domainWidget);
					((ReplaceBindingCommand) domainToWidgetBindingCommand)
							.setExistingBinding(domainToWidgetCurrentBinding);
					Binding newBinding = (Binding) EcoreUtil
							.copy(domainToWidgetCurrentBinding);
					updateDomainBoundPropertyInBindingFor(newBinding,
							domainWidget.eClass(), textfieldValue,
							BindingDirection.DOMAIN_TO_WIDGET);
					((ReplaceBindingCommand) domainToWidgetBindingCommand)
							.setNewBinding(newBinding);

					// TODO : localize it
					domainToWidgetBindingCommand.setLabel("Update Binding"); //$NON-NLS-1$ 
				}
			}
		}

		if (textfieldValue == null || "".equals(textfieldValue)) { //$NON-NLS-1$
			if (widgetToDomainCurrentBinding == null)
				return;
			else {
				widgetToDomainBindingCommand = new RemoveBindingCommand();
				((RemoveBindingCommand) widgetToDomainBindingCommand)
						.setEventDispatcher(domainWidget);
				((RemoveBindingCommand) widgetToDomainBindingCommand)
						.setBinding(widgetToDomainCurrentBinding);
				// TODO : localize it
				widgetToDomainBindingCommand.setLabel("Remove Binding"); //$NON-NLS-1$ 
			}
		} else {
			if (widgetToDomainCurrentBinding == null) {
				widgetToDomainCurrentBinding = createDefaultBindingFor(
						domainWidget.eClass(), textfieldValue,
						BindingDirection.WIDGET_TO_DOMAIN);
				if (widgetToDomainCurrentBinding != null) {
					widgetToDomainBindingCommand = new InsertNewBindingCommand();
					((InsertNewBindingCommand) widgetToDomainBindingCommand)
							.setEventDispatcher(domainWidget);
					((InsertNewBindingCommand) widgetToDomainBindingCommand)
							.setBinding(widgetToDomainCurrentBinding);
					// TODO : localize it
					widgetToDomainBindingCommand.setLabel("Add Binding"); //$NON-NLS-1$ 
				}
			} else {
				if (isSameBinding(
						domainWidget.eClass(),
						textfieldValue,
						getEventDispatcherDefaultPropertyBoundFor(domainWidget.eClass(),
								BindingDirection.WIDGET_TO_DOMAIN), null,
						widgetToDomainCurrentBinding,
						BindingDirection.WIDGET_TO_DOMAIN))
					return;
				else {
					widgetToDomainBindingCommand = new ReplaceBindingCommand();
					((ReplaceBindingCommand) widgetToDomainBindingCommand)
							.setEventDispatcher(domainWidget);
					((ReplaceBindingCommand) widgetToDomainBindingCommand)
							.setExistingBinding(widgetToDomainCurrentBinding);
					Binding newBinding = (Binding) EcoreUtil
							.copy(widgetToDomainCurrentBinding);
					updateDomainBoundPropertyInBindingFor(newBinding,
							domainWidget.eClass(), textfieldValue,
							BindingDirection.WIDGET_TO_DOMAIN);
					((ReplaceBindingCommand) widgetToDomainBindingCommand)
							.setNewBinding(newBinding);

					// TODO : localize it
					widgetToDomainBindingCommand.setLabel("Update Binding"); //$NON-NLS-1$ 
				}
			}
		}

		Command command = null;
		if (widgetToDomainBindingCommand != null
				&& domainToWidgetBindingCommand != null) {
			command = new CompoundCommand();
			((CompoundCommand) command).add(widgetToDomainBindingCommand);
			((CompoundCommand) command).add(domainToWidgetBindingCommand);
			command.setLabel(widgetToDomainBindingCommand.getLabel());
		} else if (widgetToDomainBindingCommand == null)
			command = domainToWidgetBindingCommand;
		else if (domainToWidgetBindingCommand == null)
			command = widgetToDomainBindingCommand;

		if (command != null)
			editDomain.getCommandStack().execute(command);

	}


}
