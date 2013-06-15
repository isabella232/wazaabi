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

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.edp.adapters.ContentChangedEventAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.PropertyChangedEventAdapter;
import org.eclipse.wazaabi.mm.edp.events.ContentChangedEvent;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;

public class EDPEventAdapterFactory implements EventAdapterFactory {

//	private final Logger logger = LoggerFactory
//			.getLogger(EDPEventAdapterFactory.class);

	public boolean isFactoryFor(Object callingContext, Object source, Object creationHint) {
		if (source instanceof PropertyChangedEvent
				|| source instanceof ContentChangedEvent)
			return true;
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	@Override
	public Adapter createAdapter(Object callingContext, EObject model,
			Object creationHint) {
		if (model instanceof PropertyChangedEvent)
			return new PropertyChangedEventAdapter();
		if (model instanceof ContentChangedEvent)
			return new ContentChangedEventAdapter();
		return null;
	}

}
