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

package org.eclipse.wazaabi.engine.core.adapters;

import org.eclipse.wazaabi.engine.core.editparts.CollectionEditPart;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventDispatcherAdapter;
import org.eclipse.wazaabi.mm.edp.events.Event;

public class RefreshActionAdapter extends EventHandlerAdapter {

	@Override
	public void trigger(Event event) {
		if (getEventDispatcherAdapter() instanceof CollectionEditPart)
			((CollectionEditPart) getEventDispatcherAdapter()).refresh();
	}

	@Override
	protected void eventAdded(Event event) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void eventRemoved(Event event) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void eventDispatcherAdapterAttached(
			EventDispatcherAdapter eventDispatcherAdapter) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void eventDispatcherAdapterDetached(
			EventDispatcherAdapter eventDispatcherAdapter) {
		// TODO Auto-generated method stub

	}

//	@Override
//	protected void eventPathModified(Event event, String oldPath, String newPath) {
//		// TODO Auto-generated method stub
//
//	}
}
