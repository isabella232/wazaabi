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

import org.eclipse.gef.EditPart;
import org.eclipse.gef.Request;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.UnexecutableCommand;
import org.eclipse.gef.editpolicies.AbstractEditPolicy;
import org.eclipse.gef.requests.ChangeBoundsRequest;

public class TreeEditPolicy extends AbstractEditPolicy {

	public Command getCommand(Request req) {
		if (REQ_MOVE.equals(req.getType()))
			return getMoveCommand((ChangeBoundsRequest) req);
		return null;
	}

	protected Command getMoveCommand(ChangeBoundsRequest req) {
		EditPart parent = getHost().getParent();
		if (parent != null) {
			ChangeBoundsRequest request = new ChangeBoundsRequest(
					REQ_MOVE_CHILDREN);
			request.setEditParts(getHost());
			request.setLocation(req.getLocation());
			return parent.getCommand(request);
		}
		return UnexecutableCommand.INSTANCE;
	}

}
