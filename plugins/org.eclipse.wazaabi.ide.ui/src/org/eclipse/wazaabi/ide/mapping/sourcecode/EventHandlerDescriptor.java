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

package org.eclipse.wazaabi.ide.mapping.sourcecode;

import java.util.List;

import org.eclipse.wazaabi.mm.edp.events.Event;

public class EventHandlerDescriptor extends CompilationUnitDescriptor {

	private final List<Event> events;

	public EventHandlerDescriptor(List<Event> events, String name,
			String contents) {
		super(name, contents);
		this.events = events;
	}

	public List<Event> getEvents() {
		return events;
	}

}
