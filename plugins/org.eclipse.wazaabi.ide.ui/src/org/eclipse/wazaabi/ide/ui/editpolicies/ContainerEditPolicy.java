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

import java.util.List;

import org.eclipse.gef.EditPart;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.gef.requests.CreateRequest;
import org.eclipse.gef.requests.GroupRequest;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.ide.ui.editparts.commands.OrphanChildCommand;

public class ContainerEditPolicy extends
		org.eclipse.gef.editpolicies.ContainerEditPolicy

{

	protected Command getCreateCommand(CreateRequest request) {
		return null;
	}

	public Command getOrphanChildrenCommand(GroupRequest request) {
		List<?> parts = request.getEditParts();
		CompoundCommand result = new CompoundCommand("OrphanCommand");
		for (int i = 0; i < parts.size(); i++) {
			OrphanChildCommand orphan = new OrphanChildCommand();
			orphan.setChild((AbstractComponent) ((EditPart) parts.get(i))
					.getModel());
			orphan.setParent((Container) getHost().getModel());
			orphan.setLabel("OrphanCommand");
			result.add(orphan);
		}
		return result.unwrap();
	}

}
