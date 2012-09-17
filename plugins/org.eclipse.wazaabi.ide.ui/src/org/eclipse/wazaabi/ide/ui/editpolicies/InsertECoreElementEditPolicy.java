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

import org.eclipse.gef.Request;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.editpolicies.AbstractEditPolicy;

public class InsertECoreElementEditPolicy extends AbstractEditPolicy {

	public static final String INSERT_ECORE_TARGET_ROLE = "InsertECoreTargetRole"; //$NON-NLS-1$

	@Override
	public Command getCommand(Request request) {
		if (request instanceof InsertTransformedMetamodelElementRequest)
			return getInsertECoreElementCommand((InsertTransformedMetamodelElementRequest) request);
		return super.getCommand(request);
	}

	public Command getInsertECoreElementCommand(
			InsertTransformedMetamodelElementRequest request) {
		// if (request.getNewObject() instanceof EObject) {
		// EObject child = (EObject) request.getNewObject();
		// int index = findIndexOfTreeItemAt(request.getLocation());
		//			return createCreateCommand(child, index, "Create ...");//$NON-NLS-1$
		// }
		return null;
	}

}
