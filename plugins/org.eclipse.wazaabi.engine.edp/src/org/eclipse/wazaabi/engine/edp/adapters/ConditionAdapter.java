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
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ConditionAdapter extends ValidatorAdapter {

	private static final MethodSignature[] METHOD_SIGNATURES = new MethodSignature[] { new MethodSignature(
			"canExecute", new String[] { "eventDispatcher", "eventHandler",
					"event" }, new Class[] { EventDispatcher.class,
					EventHandler.class, Event.class }, boolean.class) };

	@Override
	public void trigger(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) throws OperationAborted {
		if (!canExecute(eventDispatcher, eventHandler, event))
			throw new OperationAborted(this);
	}

	public boolean canExecute(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) {
		Object returnedValue = null;
		if (getMethodDescriptor(0) != null) {
			returnedValue = getCodeDescriptor().invokeMethod(
					getMethodDescriptor(0),
					new Object[] { eventDispatcher, eventHandler, event });
			if (returnedValue != null) {
				if (Boolean.FALSE.equals(returnedValue))
					return false;
				if (Boolean.TRUE.equals(returnedValue))
					return true;
			}
		}
		return true;
	}

	@Override
	public boolean isAdapterForType(Object type) {
		return type instanceof Condition;
	}

	public MethodSignature[] getMethodSignatures() {
		return METHOD_SIGNATURES;
	}
}
