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
import org.eclipse.wazaabi.ide.mapping.sourcecode.CompilationUnitDescriptor;
import org.eclipse.wazaabi.ide.ui.editors.WazaabiTreeEditor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ModelDescriptor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ModelDescriptorTransfert;
import org.eclipse.wazaabi.ide.ui.editparts.commands.jdt.InsertNewCompilationUnitCommand;
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
		CompoundCommand compoundCommand = new CompoundCommand();

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
							@SuppressWarnings({ "unchecked" })
							List<CompilationUnitDescriptor> compilationUnitDescriptors = (List<CompilationUnitDescriptor>) MappingUtils
									.getFFactory().get(target,
											IPackageFragment.class, 0,
											realSource,
											CompilationUnitDescriptor.class,
											null);
							for (CompilationUnitDescriptor compilationUnitDescriptor : compilationUnitDescriptors) {
								InsertNewCompilationUnitCommand command = new InsertNewCompilationUnitCommand();
								command.setCompilationUnitDescriptor(compilationUnitDescriptor);
								command.setPackageFRagment(pf);
								compoundCommand.add(command);
							}
						}
					}
				}
		}

		return compoundCommand;
	}
}