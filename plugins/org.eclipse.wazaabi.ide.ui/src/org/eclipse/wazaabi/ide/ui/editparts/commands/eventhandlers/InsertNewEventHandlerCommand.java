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

public class InsertNewEventHandlerCommand extends
		TransactionalEditingDomainCommand {

	private EventHandler newEventHandler = null;
	private EventDispatcher eventDispatcher = null;

	private int index = -1;

	public InsertNewEventHandlerCommand() {
		super("Insert New EventHandler");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getEventDispatcher() != null
				&& getNewEventHandler() != null && getIndex() >= 0
				|| getIndex() < getEventDispatcher().getHandlers().size();
	}

	@Override
	public boolean canUndo() {
		return super.canUndo();
	}

	@Override
	protected void doExecute() {
		doRedo();
	}

	@Override
	protected void doRedo() {
		System.out.println(getNewEventHandler());
		if (getIndex() == -1)
			getEventDispatcher().getHandlers().add(getNewEventHandler());
		else
			getEventDispatcher().getHandlers().add(getIndex(),
					getNewEventHandler());
	}

	@Override
	protected void doUndo() {
		getEventDispatcher().getHandlers().remove(getNewEventHandler());
	}

	public int getIndex() {
		return index;
	}

	public EventHandler getNewEventHandler() {
		return newEventHandler;
	}

	public EventDispatcher getEventDispatcher() {
		return eventDispatcher;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setNewEventHandler(EventHandler newEventHandler) {
		this.newEventHandler = newEventHandler;
	}

	public void setEventDispatcher(EventDispatcher eventDispatcher) {
		this.eventDispatcher = eventDispatcher;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(eventDispatcher));
	}

}
