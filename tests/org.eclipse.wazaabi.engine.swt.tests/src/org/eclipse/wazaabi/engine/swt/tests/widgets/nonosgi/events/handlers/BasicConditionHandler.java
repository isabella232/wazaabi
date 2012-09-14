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

package org.eclipse.wazaabi.engine.swt.tests.widgets.nonosgi.events.handlers;

import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class BasicConditionHandler {
	boolean disposed = true;

	public BasicConditionHandler() {
		System.out.println("creating " + getClass().getName());
		disposed = false;
	}

	public boolean canExecute (EventDispatcher dispatcher, EventHandler handler, Event event) {
		System.out.println("condition evaluated");
		Object context = dispatcher.get("condition");
		if (context == null) {
			dispatcher.set("condition", "condition executed");
			return true;
		} else {
			throw new OperationAborted("Aborted");
		}
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
		disposed = true;
	}
	
	public boolean isDisposed() {
		return disposed;
	}
}
