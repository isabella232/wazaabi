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

package org.eclipse.wazaabi.ide.propertysheets.graphicalhelpers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.EventDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.EventDescriptorFactory;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class EventHandlerGraphicalHelper extends AbstractGraphicalHelper {

	private static EventDescriptorFactory factory = new EventDescriptorFactory();

	@Override
	public void paint(Event event, Object element, int columnIndex) {
		EventHandler eventHandler = (EventHandler) element;
		List<String> eventLabels = new ArrayList<String>(eventHandler
				.getEvents().size());
		for (org.eclipse.wazaabi.mm.edp.events.Event ev : eventHandler
				.getEvents()) {
			EventDescriptor descriptor = (EventDescriptor) factory
					.getDescriptor(ev);
			if (descriptor != null)
				eventLabels.add(descriptor.getLabel());
			else
				eventLabels.add(ev.getId());
		}

		Rectangle bounds = ((TableItem) event.item).getBounds(columnIndex);

		String text = getEventListClause(eventHandler)
				+ getOtherClause(eventHandler);

		Point point = event.gc.stringExtent(text);

		int x = bounds.x + 3;
		int y = bounds.y + bounds.height / 2 - point.y / 2;
		event.gc.drawText(text, x, y);
	}

	protected String getEventListClause(EventHandler eventHandler) {
		List<String> eventLabels = new ArrayList<String>(eventHandler
				.getEvents().size());
		for (org.eclipse.wazaabi.mm.edp.events.Event ev : eventHandler
				.getEvents()) {
			EventDescriptor descriptor = (EventDescriptor) factory
					.getDescriptor(ev);
			if (descriptor != null)
				eventLabels.add(descriptor.getLabel());
			else if (ev instanceof PropertyChangedEvent)
				eventLabels.add(((PropertyChangedEvent) ev).getPath());
			else
				eventLabels.add(ev.getId());
		}
		String result = "";
		if (!eventLabels.isEmpty())
			result += "on ";
		for (int i = 0; i < eventLabels.size(); i++) {
			result += eventLabels.get(i);
			if (i < eventLabels.size() - 1)
				result += ", ";
			else
				result += ": ";
		}
		return result;
	}

	protected String getOtherClause(EventHandler eventHandler) {
		return eventHandler.getUri();
	}
}
