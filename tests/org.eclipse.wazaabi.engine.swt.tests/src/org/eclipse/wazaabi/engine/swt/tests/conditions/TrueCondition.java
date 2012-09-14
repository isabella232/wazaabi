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

import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class TrueCondition {

	public TrueCondition() {
		System.out.println("creating " + getClass().getName());
	}

	public boolean check(EventDispatcher dispatcher,
			EventHandler eventHandler, Event event) {
		
		System.out.println("condition evaluated");
		//return Boolean.TRUE;
		return true;
//		return dispatcher.getText().equals("");
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
