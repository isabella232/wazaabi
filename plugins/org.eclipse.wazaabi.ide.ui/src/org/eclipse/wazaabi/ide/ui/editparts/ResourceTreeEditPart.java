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

package org.eclipse.wazaabi.ide.ui.editparts;

import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.gef.EditPolicy;
import org.eclipse.wazaabi.ide.ui.editpolicies.TreeContainerEditPolicy;

public class ResourceTreeEditPart extends AbstractTreeEditPart {

	protected void createEditPolicies() {
		installEditPolicy(EditPolicy.TREE_CONTAINER_ROLE,
				new TreeContainerEditPolicy());
	}

	protected List<?> getModelChildren() {
		return ((Resource) getModel()).getContents();
	}

}
