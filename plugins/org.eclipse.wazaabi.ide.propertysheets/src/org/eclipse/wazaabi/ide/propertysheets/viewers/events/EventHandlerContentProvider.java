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

package org.eclipse.wazaabi.ide.propertysheets.viewers.events;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class EventHandlerContentProvider implements IStructuredContentProvider {

	private final EventHandler blankHandlerForInsertion;

	public EventHandlerContentProvider(EventHandler blankHandlerForInsertion) {
		this.blankHandlerForInsertion = blankHandlerForInsertion;
	}

	public void dispose() {
	}

	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof EventDispatcher) {
			List<EventHandler> eventHandlers = new ArrayList<EventHandler>(
					((EventDispatcher) inputElement).getHandlers());
			eventHandlers.add(blankHandlerForInsertion);
			return eventHandlers.toArray();
		}
		return new Object[] {};
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
}
