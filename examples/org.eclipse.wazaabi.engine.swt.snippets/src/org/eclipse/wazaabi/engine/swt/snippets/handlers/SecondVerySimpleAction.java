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

package org.eclipse.wazaabi.engine.swt.snippets.handlers;

import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class SecondVerySimpleAction {

	public SecondVerySimpleAction() {
		System.out.println("creating " + getClass().getName());
	}

	public void execute(Widget dispatcher,
			EventHandler eventHandler, Event event) {
		if (dispatcher instanceof PushButton)
			((PushButton) dispatcher).setText("Hello universe");
		System.out.println("Youhou");
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
