/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editparts.commands.eventhandlers;

import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class RemoveEventHandlerCommand extends
		TransactionalEditingDomainCommand {

	private EventHandler eventHandler = null;
	private EventDispatcher eventDispatcher = null;

	private int index = -1;

	public RemoveEventHandlerCommand() {
		super("Remove EventHandler");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getEventDispatcher() != null
				&& getEventHandler() != null;
	}

	@Override
	public boolean canUndo() {
		return super.canUndo() & index != -1;
	}

	@Override
	protected void doExecute() {
		index = getEventDispatcher().getHandlers().indexOf(getEventHandler());
		doRedo();
	}

	@Override
	protected void doRedo() {
		getEventDispatcher().getHandlers().remove(getEventHandler());
	}

	@Override
	protected void doUndo() {
		getEventDispatcher().getHandlers().add(index, getEventHandler());
	}

	public EventHandler getEventHandler() {
		return eventHandler;
	}

	public EventDispatcher getEventDispatcher() {
		return eventDispatcher;
	}

	public void setEventHandler(EventHandler eventHandler) {
		this.eventHandler = eventHandler;
	}

	public void setEventDispatcher(EventDispatcher eventDispatcher) {
		this.eventDispatcher = eventDispatcher;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(eventDispatcher));
	}

}
