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

package org.eclipse.wazaabi.ide.ui.editparts.commands.components;

import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class DeleteComponentCommand extends TransactionalEditingDomainCommand {

	private AbstractComponent child;
	private Container container;
	private int index = -1;

	public DeleteComponentCommand() {
		super("Delete");
	}

	public void setChild(AbstractComponent child) {
		this.child = child;
	}

	public void setContainer(Container container) {
		this.container = container;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(container));
	}

	@Override
	protected void doExecute() {
		index = getContainer().getChildren().indexOf(getChild());
		doRedo();
	}

	@Override
	protected void doRedo() {
		getContainer().getChildren().remove(getChild());
	}

	@Override
	protected void doUndo() {
		getContainer().getChildren().add(getIndex(), getChild());
	}

	public int getIndex() {
		return index;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public AbstractComponent getChild() {
		return child;
	}

	public Container getContainer() {
		return container;
	}

}
