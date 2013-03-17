/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
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

import java.util.List;

import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class InsertNewEventHandlerCommand extends
		TransactionalEditingDomainCommand {

	private EventDispatcher eventDispatcher = null;
	private List<Event> events = null;
	private String uri = null;

	private EventHandler eventHandler = null;

	private int index = -1;

	public InsertNewEventHandlerCommand() {
		super("InsertNewEventHandlerCommand"); // TODO : localize that
	}

	public boolean canExecute() {
		return getEventDispatcher() != null && super.canExecute();
	}

	@Override
	protected void doExecute() {
		eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		if (getUri() != null && !getUri().isEmpty())
			eventHandler.setUri(getUri());
		if (getEvents() != null && !getEvents().isEmpty())
			eventHandler.getEvents().addAll(getEvents());
		doRedo();
	}

	@Override
	public boolean canUndo() {
		return getEventDispatcher() != null && eventHandler != null
				&& getEventDispatcher().getHandlers().contains(eventHandler)
				&& super.canUndo();
	}

	@Override
	protected void doRedo() {
		if (getIndex() != -1)
			getEventDispatcher().getHandlers().add(getIndex(), eventHandler);
		else
			getEventDispatcher().getHandlers().add(eventHandler);
	}

	@Override
	protected void doUndo() {
		getEventDispatcher().getHandlers().remove(eventHandler);
	}

	public EventDispatcher getEventDispatcher() {
		return eventDispatcher;
	}

	public List<Event> getEvents() {
		return events;
	}

	public int getIndex() {
		return index;
	}

	public String getUri() {
		return uri;
	}

	public void setEventDispatcher(EventDispatcher eventDispatcher) {
		this.eventDispatcher = eventDispatcher;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(eventDispatcher));
	}

	public void setEvents(List<Event> events) {
		this.events = events;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setUri(String uri) {
		this.uri = uri;
	}

}
