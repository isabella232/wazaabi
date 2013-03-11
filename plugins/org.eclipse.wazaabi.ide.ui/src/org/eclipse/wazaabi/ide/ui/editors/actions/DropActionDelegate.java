/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editors.actions;

import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CommandStack;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.IDropActionDelegate;
import org.eclipse.wazaabi.ide.mapping.rules.MappingUtils;
import org.eclipse.wazaabi.ide.mapping.sourcecode.EventHandlerDescriptor;
import org.eclipse.wazaabi.ide.ui.editors.WazaabiTreeEditor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ModelDescriptor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ModelDescriptorTransfert;
import org.eclipse.wazaabi.ide.ui.editparts.commands.eventhandlers.InsertNewEventHandlerCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.jdt.InsertNewCompilationUnitCommand;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DropActionDelegate implements IDropActionDelegate {
	final static Logger logger = LoggerFactory
			.getLogger(DropActionDelegate.class);

	public boolean run(Object source, Object target) {

		if (PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null
				&& PlatformUI.getWorkbench().getActiveWorkbenchWindow()
						.getActivePage() != null) {
			if (PlatformUI.getWorkbench().getActiveWorkbenchWindow()
					.getActivePage().getActivePart() instanceof WazaabiTreeEditor) {
				CommandStack commandStack = ((WazaabiTreeEditor) PlatformUI
						.getWorkbench().getActiveWorkbenchWindow()
						.getActivePage().getActivePart()).getViewer()
						.getEditDomain().getCommandStack();
				if (commandStack != null) {
					Command command = getCommand(source, target);
					if (command != null && command.canExecute()) {
						commandStack.execute(command);
						return true;
					}
				} else
					logger.debug("unable to resolve the commandStack"); //$NON-NLS-1$
			} else {
				// NOTHING TO DO UNTIL other kind of parts (outline ViewPart)
				// could provide D&D
			}
		}
		return false;
	}

	protected Command getCommand(Object source, Object target) {
		CompoundCommand resultCommand = new CompoundCommand();

		if (target instanceof IPackageFragment) {
			IPackageFragment pf = (IPackageFragment) target;

			ModelDescriptor modelDescriptors[] = ModelDescriptorTransfert
					.getInstance().fromByteArray((byte[]) source);
			if (modelDescriptors != null)
				for (ModelDescriptor modelDescriptor : modelDescriptors) {
					TransactionalEditingDomain t = TransactionalEditingDomain.Registry.INSTANCE
							.getEditingDomain(modelDescriptor
									.getEditingDomainId());
					if (t != null) {
						Resource r = t.getResourceSet()
								.getResource(
										URI.createURI(modelDescriptor
												.getResourceURI()), false);
						if (r != null) {
							EObject realSource = r.getEObject(modelDescriptor
									.getUriFragment());
							if (realSource instanceof EventDispatcher) {
								EventDispatcher eventDispatcher = (EventDispatcher) realSource;
								@SuppressWarnings({ "unchecked" })
								List<EventHandlerDescriptor> eventHandlerDescriptors = (List<EventHandlerDescriptor>) MappingUtils
										.getFFactory().get(target,
												IPackageFragment.class, 0,
												eventDispatcher,
												EventHandlerDescriptor.class,
												null);
								for (EventHandlerDescriptor eventHandlerDescriptor : eventHandlerDescriptors) {
									CompoundCommand compoundCommand = new CompoundCommand();

									InsertNewCompilationUnitCommand insertNewCompilationUnitCommand = new InsertNewCompilationUnitCommand();
									insertNewCompilationUnitCommand
											.setCompilationUnitDescriptor(eventHandlerDescriptor);
									insertNewCompilationUnitCommand
											.setPackageFRagment(pf);
									compoundCommand
											.add(insertNewCompilationUnitCommand);

									InsertNewEventHandlerCommand insertNewEventHandlerCommand = new InsertNewEventHandlerCommand();
									insertNewEventHandlerCommand
											.setEventDispatcher(eventDispatcher);

									insertNewEventHandlerCommand
											.setUri(createURI(pf,
													eventHandlerDescriptor));
									insertNewEventHandlerCommand
											.setEvents(eventHandlerDescriptor
													.getEvents());

									compoundCommand
											.add(insertNewEventHandlerCommand);

									resultCommand.add(compoundCommand);
								}
							}
						}
					}
				}
		}

		return resultCommand;
	}

	protected String createURI(IPackageFragment pf,
			EventHandlerDescriptor eventHandlerDescriptor) {

		// TODO : at the moment we assume that the project name is the bundle
		// symbolic name
		// TODO : we also assume that only platform:/plugin is used

		return "platform:/plugin/"
				+ pf.getJavaProject().getProject().getName()
				+ "/"
				+ pf.getElementName()
				+ "."
				+ eventHandlerDescriptor.getName().substring(
						0,
						eventHandlerDescriptor.getName().length()
								- ".java".length());
	}
}