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

package org.eclipse.wazaabi.engine.edp.adapters;

import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ActionAdapterImpl extends AbstractOperationAdapter implements
		ActionAdapter {

	private static final MethodSignature[] METHOD_SIGNATURES = new MethodSignature[] { new MethodSignature(
			"execute", new String[] { "eventDispatcher", "eventHandler",
					"event" }, new Class[] { EventDispatcher.class,
					EventHandler.class, Event.class }, null) };

	public void trigger(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) throws OperationAborted {
		if (getMethodDescriptor(0) != null)
			getCodeDescriptor().invokeMethod(getMethodDescriptor(0),
					new Object[] { eventDispatcher, eventHandler, event });
	}

	@Override
	public boolean isAdapterForType(Object type) {
		return type instanceof Action;
	}

	public MethodSignature[] getMethodSignatures() {
		return METHOD_SIGNATURES;
	}

	public String getErrorMessage() {
		return null;
	}

}
