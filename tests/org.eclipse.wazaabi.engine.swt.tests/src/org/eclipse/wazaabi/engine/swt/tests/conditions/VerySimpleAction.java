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

package org.eclipse.wazaabi.engine.swt.tests.conditions;


import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class VerySimpleAction {

	public VerySimpleAction() {
		System.out.println("creating " + getClass().getName());
	}

	public void execute(EventDispatcher dispatcher,
			EventHandler eventHandler, Event event) {
		System.out.println("Running action");
		((Label)(dispatcher)).setText("newText");
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
