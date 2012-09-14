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

package org.eclipse.wazaabi.engine.swt.snippets.conditions;

import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class BadCondition {

	public BadCondition() {
		System.out.println("creating " + getClass().getName());
	}

	public boolean canExecute(EventDispatcher dispatcher,
			EventHandler eventHandler, Event event) {
		System.out.println("condition evaluated");
		//throw new OperationAborted("Bad bad bad bad");
		return false;
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
	
	public String errorMessage() {
		return "Your data is not valid";
	}
}
