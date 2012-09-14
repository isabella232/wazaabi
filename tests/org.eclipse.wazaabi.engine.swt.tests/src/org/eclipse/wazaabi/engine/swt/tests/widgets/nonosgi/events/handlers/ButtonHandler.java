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

import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ButtonHandler {

	private int counter = 0;
	private boolean disposed = false;

	public void dispose() {
		disposed = true;
	}

	public void execute(PushButton dispatcher,
			EventHandler eventHandler, Event event) {
		incrementCounter();
	}

	public int getCounter() {
		return counter;
	}

	public void incrementCounter() {
		counter++;
	}

	public boolean isDisposed() {
		return disposed;
	}

}
