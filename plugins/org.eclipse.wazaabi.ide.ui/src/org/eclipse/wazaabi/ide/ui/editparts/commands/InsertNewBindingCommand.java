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

import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;

public class InsertNewBindingCommand extends TransactionalEditingDomainCommand {

	private Binding binding;
	private EventDispatcher eventDispatcher;
	private int index = -1;

	public InsertNewBindingCommand() {
		super("InsertNewBindingCommand"); // TODO : localize that
	}

	public boolean canExecute() {
		return getBinding() != null && getEventDispatcher() != null
				&& super.canExecute();
	}

	@Override
	protected void doExecute() {
		doRedo();
	}

	@Override
	protected void doRedo() {
		if (getIndex() != -1)
			getEventDispatcher().getHandlers().add(getIndex(), getBinding());
		else
			getEventDispatcher().getHandlers().add(getBinding());
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setEventDispatcher(EventDispatcher eventDispatcher) {
		this.eventDispatcher = eventDispatcher;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(eventDispatcher));
	}

	@Override
	protected void doUndo() {
		getEventDispatcher().getHandlers().remove(getBinding());
	}

	public EventDispatcher getEventDispatcher() {
		return eventDispatcher;
	}

	public int getIndex() {
		return index;
	}

	public Binding getBinding() {
		return binding;
	}

	public void setBinding(Binding binding) {
		this.binding = binding;
	}

}
