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

import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;


public class ConditionAdapter extends ValidatorAdapter {
	
	public ConditionAdapter() {
		this.executeMethodName = "canExecute";
	}

	
	@Override
	protected void registerMethods(AbstractCodeDescriptor codeDescriptor) {
		setExecuteMethodDescriptor(codeDescriptor.getMethodDescriptor(executeMethodName,
				new String[] { "eventDispatcher", "eventHandler", "event" },
				new Class[] { EventDispatcher.class,EventHandler.class, Event.class }, boolean.class)); //$NON-NLS-1$
		super.registerMethods(codeDescriptor);
	}

	@Override
	public void trigger(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) throws OperationAborted {
		boolean canExecute = this.canExecute(eventDispatcher, eventHandler, event);
		if (!canExecute) {
			throw new OperationAborted(this);
		}
	}
	
	public boolean canExecute(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) {
		Object methodReturned = null;
		if (getExecuteMethodDescriptor() != null) {
			methodReturned = getCodeDescriptor().invokeMethod(
					getExecuteMethodDescriptor(),
					new Object[] { eventDispatcher, eventHandler, event });
			if (methodReturned != null) {
				if (Boolean.FALSE.equals(methodReturned))
					return false;
				if (Boolean.TRUE.equals(methodReturned))
					return true;
			}
		}
		return true;
	}
	
	@Override
	public boolean isAdapterForType(Object type) {
		return type instanceof Condition;
	}

}
