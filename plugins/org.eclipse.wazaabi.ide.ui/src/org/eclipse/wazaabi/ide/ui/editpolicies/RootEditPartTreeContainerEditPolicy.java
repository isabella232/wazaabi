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

package org.eclipse.wazaabi.ide.ui.editpolicies;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.domain.IEditingDomainProvider;
import org.eclipse.gef.Request;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.UnexecutableCommand;
import org.eclipse.gef.requests.ChangeBoundsRequest;
import org.eclipse.gef.requests.CreateRequest;

public class RootEditPartTreeContainerEditPolicy extends
		org.eclipse.gef.editpolicies.TreeContainerEditPolicy

{

	protected Command createCreateCommand(EObject child, int index, String label) {
		// if (getHost().getModel() instanceof Container)
		// if (child instanceof AbstractComponent) {
		// InsertNewComponentCommand cmd = new InsertNewComponentCommand();
		// cmd.setContainer((Container) getHost().getModel());
		// cmd.setChild((AbstractComponent) child);
		// cmd.setLabel(label);
		// if (index >= 0)
		// cmd.setIndex(index);
		// return cmd;
		// } else if (child instanceof LayoutRule) {
		// InsertNewUniqueLayoutCommand cmd = new
		// InsertNewUniqueLayoutCommand();
		// cmd.setContainer((Container) getHost().getModel());
		// cmd.setLayout((LayoutRule) child);
		// cmd.setLabel(label);
		// return cmd;
		// }
		// if (getHost().getModel() instanceof AbstractComponent
		// && child instanceof LayoutDataRule) {
		// InsertNewUniqueLayoutDataCommand cmd = new
		// InsertNewUniqueLayoutDataCommand();
		// cmd.setComponent((AbstractComponent) getHost().getModel());
		// cmd.setLayoutData((LayoutDataRule) child);
		// cmd.setLabel(label);
		// return cmd;
		// }
		System.out.println((((IEditingDomainProvider) getHost().getViewer())
				.getEditingDomain()));
		return null;
	}

	protected Command getAddCommand(ChangeBoundsRequest request) {
		return UnexecutableCommand.INSTANCE;
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
		return UnexecutableCommand.INSTANCE;
	}

	@Override
	public void eraseTargetFeedback(Request req) {
		// TODO Auto-generated method stub
		super.eraseTargetFeedback(req);
	}

	@Override
	public void showTargetFeedback(Request req) {
		// TODO Auto-generated method stub
		super.showTargetFeedback(req);
	}

}
