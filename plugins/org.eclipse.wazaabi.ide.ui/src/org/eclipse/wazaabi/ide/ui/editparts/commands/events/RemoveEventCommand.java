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

package org.eclipse.wazaabi.ide.ui.editparts.commands.events;

import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class RemoveEventCommand extends TransactionalEditingDomainCommand {

	private Event event = null;
	private EventHandler eventHandler = null;

	private int index = -1;

	public RemoveEventCommand() {
		super("Remove Event");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getEventHandler() != null
				&& getEvent() != null;
	}

	@Override
	public boolean canUndo() {
		return super.canUndo() & index != -1;
	}

	@Override
	protected void doExecute() {
		index = getEventHandler().getEvents().indexOf(getEvent());
		doRedo();
	}

	@Override
	protected void doRedo() {
		getEventHandler().getEvents().remove(getEvent());
	}

	@Override
	protected void doUndo() {
		getEventHandler().getEvents().add(index, getEvent());
	}

	public Event getEvent() {
		return event;
	}

	public EventHandler getEventHandler() {
		return eventHandler;
	}

	public void setEvent(Event event) {
		this.event = event;
	}

	public void setEventHandler(EventHandler eventHandler) {
		this.eventHandler = eventHandler;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(eventHandler));
	}

}
