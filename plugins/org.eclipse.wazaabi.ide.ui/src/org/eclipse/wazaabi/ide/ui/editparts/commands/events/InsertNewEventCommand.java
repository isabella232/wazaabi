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

public class InsertNewEventCommand extends TransactionalEditingDomainCommand {

	private Event newEvent = null;
	private EventHandler eventHandler = null;

	private int index = -1;

	public InsertNewEventCommand() {
		super("Insert New Event");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getEventHandler() != null
				&& getNewEvent() != null && getIndex() >= 0
				|| getIndex() < getEventHandler().getEvents().size();
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
		System.out.println(getNewEvent());
		if (getIndex() == -1)
			getEventHandler().getEvents().add(getNewEvent());
		else
			getEventHandler().getEvents().add(getIndex(),
					getNewEvent());
	}

	@Override
	protected void doUndo() {
		getEventHandler().getEvents().remove(getNewEvent());
	}

	public int getIndex() {
		return index;
	}

	public Event getNewEvent() {
		return newEvent;
	}

	public EventHandler getEventHandler() {
		return eventHandler;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setNewEvent(Event newEvent) {
		this.newEvent = newEvent;
	}

	public void setEventHandler(EventHandler eventHandler) {
		this.eventHandler = eventHandler;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(eventHandler));
	}

}
