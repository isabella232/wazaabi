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

package org.eclipse.wazaabi.ide.ui.editparts.commands;

import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class ReorderComponentsCommand extends TransactionalEditingDomainCommand {

	private int oldIndex, newIndex;
	private AbstractComponent child;
	private Container container;

	public ReorderComponentsCommand(AbstractComponent child, Container parent,
			int newIndex) {
		super("reorder");
		this.child = child;
		this.container = parent;
		this.newIndex = newIndex;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(parent));
	}

	@Override
	protected void doExecute() {
		oldIndex = container.getChildren().indexOf(child);
		doRedo();
	}

	@Override
	protected void doRedo() {
		container.getChildren().remove(child);
		container.getChildren().add(newIndex, child);
	}

	@Override
	protected void doUndo() {
		container.getChildren().remove(child);
		container.getChildren().add(oldIndex, child);
	}

}
