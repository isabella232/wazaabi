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

public class ReplaceBindingCommand extends TransactionalEditingDomainCommand {

	private Binding existingBinding = null;
	private Binding newBinding = null;
	private EventDispatcher eventDispatcher;
	private int existingIndex = -1;

	public ReplaceBindingCommand() {
		super("ReplaceBindingCommand"); // TODO : localize that
	}

	public boolean canExecute() {
		return getExistingBinding() != null
				&& getNewBinding() != null
				&& getEventDispatcher() != null
				&& getEventDispatcher().getHandlers().indexOf(
						getExistingBinding()) != -1
				&& getEventDispatcher().getHandlers().indexOf(getNewBinding()) == -1
				&& super.canExecute();
	}

	@Override
	protected void doExecute() {
		existingIndex = getEventDispatcher().getHandlers().indexOf(
				getExistingBinding());
		doRedo();
	}

	@Override
	protected void doRedo() {
		getEventDispatcher().getHandlers().remove(getExistingBinding());
		getEventDispatcher().getHandlers().add(getExistingIndex(), getNewBinding());
	}

	public void setEventDispatcher(EventDispatcher eventDispatcher) {
		this.eventDispatcher = eventDispatcher;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(eventDispatcher));
	}

	@Override
	protected void doUndo() {
		getEventDispatcher().getHandlers().remove(getNewBinding());
		getEventDispatcher().getHandlers().add(getExistingIndex(), getExistingBinding());
	}

	public EventDispatcher getEventDispatcher() {
		return eventDispatcher;
	}

	public Binding getExistingBinding() {
		return existingBinding;
	}

	public void setExistingBinding(Binding existingBinding) {
		this.existingBinding = existingBinding;
	}

	protected int getExistingIndex() {
		return existingIndex;
	}

	protected void setExistingIndex(int existingIndex) {
		this.existingIndex = existingIndex;
	}

	public Binding getNewBinding() {
		return newBinding;
	}

	public void setNewBinding(Binding newBinding) {
		this.newBinding = newBinding;
	}

}
