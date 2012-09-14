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

package org.eclipse.wazaabi.engine.edp.events;

import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.events.ContentChangedEvent;
import org.eclipse.wazaabi.engine.edp.adapters.ContentChangedEventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.PropertyChangedEventAdapter;

public class EDPEventAdapterFactory implements EventAdapterFactory {

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof PropertyChangedEvent
				|| source instanceof ContentChangedEvent)
			return true;
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	public EventAdapter createEventAdapter(Object context, Event event) {
		if (event instanceof PropertyChangedEvent)
			return new PropertyChangedEventAdapter();
		if (event instanceof ContentChangedEvent)
			return new ContentChangedEventAdapter();
		return null;
	}

}
