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

public class RemoveBindingCommand extends TransactionalEditingDomainCommand {

	private Binding binding = null;
	private EventDispatcher eventDispatcher;
	private int previousIndex = -1;

	public RemoveBindingCommand() {
		super("RemoveBindingCommand"); // TODO : localize that
	}

	public boolean canExecute() {
		return getBinding() != null
				&& getEventDispatcher() != null
				&& getEventDispatcher().getHandlers().indexOf(getBinding()) != -1
				&& super.canExecute();
	}

	@Override
	protected void doExecute() {
		previousIndex = getEventDispatcher().getHandlers()
				.indexOf(getBinding());
		doRedo();
	}

	@Override
	protected void doRedo() {
		getEventDispatcher().getHandlers().remove(getBinding());
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

	public Binding getBinding() {
		return binding;
	}

	public void setBinding(Binding binding) {
		this.binding = binding;
	}

	protected int getPreviousIndex() {
		return previousIndex;
	}

	protected void setPreviousIndex(int previousIndex) {
		this.previousIndex = previousIndex;
	}

}
