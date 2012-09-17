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

import java.util.List;

import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class OrphanChildCommand extends TransactionalEditingDomainCommand {

	private Container diagram;
	private AbstractComponent child;
	private int index;

	public OrphanChildCommand() {
		super("OrphaCommand");
	}

	public void setChild(AbstractComponent child) {
		this.child = child;
	}

	public void setParent(Container parent) {
		diagram = parent;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(parent));
	}

	@Override
	protected void doExecute() {
		List<?> children = diagram.getChildren();
		index = children.indexOf(child);
		doRedo();
	}

	@Override
	protected void doRedo() {
		diagram.getChildren().remove(child);
	}

	@Override
	protected void doUndo() {
		diagram.getChildren().add(index, child);
	}

}
