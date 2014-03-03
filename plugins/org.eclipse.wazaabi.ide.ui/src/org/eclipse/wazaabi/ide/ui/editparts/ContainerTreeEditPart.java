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

package org.eclipse.wazaabi.ide.ui.editparts;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.gef.EditPolicy;
import org.eclipse.gef.RootEditPart;
import org.eclipse.gef.editpolicies.RootComponentEditPolicy;
import org.eclipse.wazaabi.ide.ui.editpolicies.ContainerEditPolicy;
import org.eclipse.wazaabi.ide.ui.editpolicies.InsertECoreElementEditPolicy;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class ContainerTreeEditPart extends AbstractComponentTreeEditPart {

	protected void createEditPolicies() {
		super.createEditPolicies();
		installEditPolicy(EditPolicy.CONTAINER_ROLE, new ContainerEditPolicy());
		installEditPolicy(
				InsertECoreElementEditPolicy.INSERT_ECORE_TARGET_ROLE,
				new InsertECoreElementEditPolicy());
		// installEditPolicy(EditPolicy.TREE_CONTAINER_ROLE,
		// new TreeContainerEditPolicy());
		// If this editpart is the contents of the viewer, then it is not
		// deletable!
		if (getParent() instanceof RootEditPart)
			installEditPolicy(EditPolicy.COMPONENT_ROLE,
					new RootComponentEditPolicy());
	}

	@SuppressWarnings("unchecked")
	protected List<?> getModelChildren() {
		List<EObject> children = new ArrayList<EObject>();
		children.addAll((List<EObject>) super.getModelChildren());
		children.addAll(((Container) getModel()).getChildren());
		return children;
	}

}
