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

import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;

public abstract class TransactionalEditingDomainCommand extends
		org.eclipse.gef.commands.Command {

	private TransactionalEditingDomain domain = null;

	public TransactionalEditingDomainCommand() {
		super();
	}

	public TransactionalEditingDomainCommand(String label) {
		super(label);
	}

	public boolean canExecute() {
		return getTransactionalEditingDomain() != null;
	};

	@Override
	public boolean canUndo() {
		return getTransactionalEditingDomain() != null;
	}

	protected abstract void doExecute();

	protected abstract void doRedo();

	protected abstract void doUndo();

	@Override
	public final void execute() {
		getTransactionalEditingDomain().getCommandStack().execute(
				new RecordingCommand(getTransactionalEditingDomain()) {
					protected void doExecute() {
						TransactionalEditingDomainCommand.this.doExecute();
					}
				});
	}

	protected TransactionalEditingDomain getTransactionalEditingDomain() {
		return domain;
	}

	@Override
	public final void redo() {
		getTransactionalEditingDomain().getCommandStack().execute(
				new RecordingCommand(getTransactionalEditingDomain()) {
					protected void doExecute() {
						TransactionalEditingDomainCommand.this.doRedo();
					}
				});
	}

	public void setTransactionalEditingDomain(
			TransactionalEditingDomain domain) {
		this.domain = domain;
	}

	@Override
	public final void undo() {
		getTransactionalEditingDomain().getCommandStack().execute(
				new RecordingCommand(getTransactionalEditingDomain()) {
					protected void doExecute() {
						TransactionalEditingDomainCommand.this.doUndo();
					}
				});
	}

}
