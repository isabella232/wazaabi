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

package org.eclipse.wazaabi.ide.ui.editpolicies;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.requests.GroupRequest;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.ide.ui.editparts.commands.components.DeleteComponentCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.components.DeleteResourceRootCommand;

public class ComponentEditPolicy extends
		org.eclipse.gef.editpolicies.ComponentEditPolicy {

	protected Command createDeleteCommand(GroupRequest request) {
		Object container = getHost().getParent().getModel();
		if (container instanceof Container
				&& getHost().getModel() instanceof AbstractComponent) {
			DeleteComponentCommand deleteCmd = new DeleteComponentCommand();
			deleteCmd.setContainer((Container) container);
			deleteCmd.setChild((AbstractComponent) getHost().getModel());
			return deleteCmd;
		} else if (container instanceof Resource
				&& getHost().getModel() instanceof AbstractComponent) {
			DeleteResourceRootCommand deleteCmd = new DeleteResourceRootCommand();
			deleteCmd.setResource((Resource) container);
			deleteCmd.setChild((AbstractComponent) getHost().getModel());
			return deleteCmd;
		}
		return null;
	}
}
