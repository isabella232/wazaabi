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

package org.eclipse.wazaabi.ide.ui.editors;

import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.ui.actions.ActionRegistry;
import org.eclipse.gef.ui.actions.GEFActionConstants;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.wazaabi.ide.ui.editors.actions.ChangeMappingAction;
import org.eclipse.wazaabi.ide.ui.editors.actions.InsertECoreElementAction;

public class WazaabiTreeEditorContextMenuProvider extends
		org.eclipse.gef.ContextMenuProvider {

	private ActionRegistry actionRegistry;

	public WazaabiTreeEditorContextMenuProvider(EditPartViewer viewer,
			ActionRegistry registry) {
		super(viewer);
		setActionRegistry(registry);
	}

	public void buildContextMenu(IMenuManager manager) {
		GEFActionConstants.addStandardActionGroups(manager);

		IAction action;

		action = getActionRegistry().getAction(ActionFactory.UNDO.getId());
		manager.appendToGroup(GEFActionConstants.GROUP_UNDO, action);

		action = getActionRegistry().getAction(ActionFactory.REDO.getId());
		manager.appendToGroup(GEFActionConstants.GROUP_UNDO, action);

		// action = getActionRegistry().getAction(ActionFactory.PASTE.getId());
		// if (action.isEnabled())
		// manager.appendToGroup(GEFActionConstants.GROUP_EDIT, action);

		action = getActionRegistry().getAction(ActionFactory.DELETE.getId());
		if (action.isEnabled())
			manager.appendToGroup(GEFActionConstants.GROUP_EDIT, action);

		action = getActionRegistry().getAction(InsertECoreElementAction.ID);
		if (action.isEnabled())
			manager.appendToGroup(GEFActionConstants.GROUP_EDIT, action);

		action = getActionRegistry().getAction(ChangeMappingAction.ID);
		if (action.isEnabled())
			manager.appendToGroup(GEFActionConstants.GROUP_EDIT, action);

	}

	private ActionRegistry getActionRegistry() {
		return actionRegistry;
	}

	private void setActionRegistry(ActionRegistry registry) {
		actionRegistry = registry;
	}

}
