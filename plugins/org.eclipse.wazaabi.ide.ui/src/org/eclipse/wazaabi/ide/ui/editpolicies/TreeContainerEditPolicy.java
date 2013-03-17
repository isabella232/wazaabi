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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.gef.commands.UnexecutableCommand;
import org.eclipse.gef.requests.ChangeBoundsRequest;
import org.eclipse.gef.requests.CreateRequest;
import org.eclipse.wazaabi.ide.ui.editparts.AbstractComponentTreeEditPart;
import org.eclipse.wazaabi.ide.ui.editparts.ResourceTreeEditPart;
import org.eclipse.wazaabi.ide.ui.editparts.commands.InsertNewUniqueLayoutCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.InsertNewUniqueLayoutDataCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.ReorderComponentsCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.components.InsertNewComponentCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.components.SetRootInResourceCommand;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class TreeContainerEditPolicy extends
		org.eclipse.gef.editpolicies.TreeContainerEditPolicy

{

	protected Command createCreateCommand(EObject child, int index, String label) {
		if (getHost().getModel() instanceof Resource
				&& child instanceof AbstractComponent) {
			SetRootInResourceCommand cmd = new SetRootInResourceCommand();
			cmd.setResource((Resource) getHost().getModel());
			cmd.setChild((AbstractComponent) child);
			return cmd;
		}
		if (getHost().getModel() instanceof Container)
			if (child instanceof AbstractComponent) {
				InsertNewComponentCommand cmd = new InsertNewComponentCommand();
				cmd.setContainer((Container) getHost().getModel());
				cmd.setChild((AbstractComponent) child);
				cmd.setLabel(label);
				if (index >= 0)
					cmd.setIndex(index);
				return cmd;
			} else if (child instanceof LayoutRule) {
				InsertNewUniqueLayoutCommand cmd = new InsertNewUniqueLayoutCommand();
				cmd.setContainer((Container) getHost().getModel());
				cmd.setLayout((LayoutRule) child);
				cmd.setLabel(label);
				return cmd;
			}
		if (getHost().getModel() instanceof AbstractComponent
				&& child instanceof LayoutDataRule) {
			InsertNewUniqueLayoutDataCommand cmd = new InsertNewUniqueLayoutDataCommand();
			cmd.setComponent((AbstractComponent) getHost().getModel());
			cmd.setLayoutData((LayoutDataRule) child);
			cmd.setLabel(label);
			return cmd;
		}

		return null;
	}

	protected Command getAddCommand(ChangeBoundsRequest request) {
		CompoundCommand command = new CompoundCommand();
		command.setDebugLabel("");//$NON-NLS-1$
		List<?> editparts = request.getEditParts();
		int index = findIndexOfTreeItemAt(request.getLocation());

		for (int i = 0; i < editparts.size(); i++) {
			EditPart child = (EditPart) editparts.get(i);
			if (isAncestor(child, getHost()))
				command.add(UnexecutableCommand.INSTANCE);
			else {
				EObject childModel = (EObject) child.getModel();
				command.add(createCreateCommand(childModel, index, ""));//$NON-NLS-1$
			}
		}
		return command;
	}

	protected Command getCreateCommand(CreateRequest request) {
		if (request.getNewObject() instanceof EObject) {
			EObject child = (EObject) request.getNewObject();
			int index = findIndexOfTreeItemAt(request.getLocation());
			return createCreateCommand(child, index, "Create ...");//$NON-NLS-1$
		}
		return null;
	}

	protected Command getMoveChildrenCommand(ChangeBoundsRequest request) {
		if (getHost() instanceof ResourceTreeEditPart)
			return UnexecutableCommand.INSTANCE;
		CompoundCommand command = new CompoundCommand();
		List<?> editparts = request.getEditParts();
		List<?> children = getHost().getChildren();
		int newIndex = findIndexOfTreeItemAt(request.getLocation());

		int nonAbstractComponentEditPartCount = 0;
		for (int i = 0; i < newIndex; i++)
			if (!(children.get(i) instanceof AbstractComponentTreeEditPart))
				nonAbstractComponentEditPartCount++;
		newIndex -= nonAbstractComponentEditPartCount;
		for (int i = 0; i < editparts.size(); i++) {
			EditPart child = (EditPart) editparts.get(i);
			int tempIndex = newIndex;
			int oldIndex = children.indexOf(child)
					- nonAbstractComponentEditPartCount;
			if (oldIndex == tempIndex || oldIndex + 1 == tempIndex) {
				command.add(UnexecutableCommand.INSTANCE);
				return command;
			} else if (oldIndex <= tempIndex) {
				tempIndex--;
			}
			command.add(new ReorderComponentsCommand((AbstractComponent) child
					.getModel(), (Container) getHost().getModel(), tempIndex));
		}
		return command;
	}

	protected boolean isAncestor(EditPart source, EditPart target) {
		if (source == target)
			return true;
		if (target.getParent() != null)
			return isAncestor(source, target.getParent());
		return false;
	}

}
