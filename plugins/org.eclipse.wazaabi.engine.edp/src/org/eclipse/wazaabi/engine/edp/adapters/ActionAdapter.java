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
import org.eclipse.wazaabi.engine.edp.coderesolution.DeferredAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ActionAdapter extends OperationAdapter implements
		DeferredAdapter {
	
	public ActionAdapter() {
		executeMethodName = "execute";
	}
	
	protected void registerMethods(AbstractCodeDescriptor codeDescriptor) {
		if (this.getClass().toString().equalsIgnoreCase("class org.eclipse.wazaabi.engine.edp.adapters.ActionAdapter")) {
		setExecuteMethodDescriptor(codeDescriptor.getMethodDescriptor(executeMethodName,
				new String[] { "eventDispatcher", "eventHandler", "event" },
				new Class[] { EventDispatcher.class,
						EventHandler.class, Event.class }, null)); //$NON-NLS-1$
		}
		super.registerMethods(codeDescriptor);
	}

	public void trigger(EventDispatcher eventDispatcher, EventHandler eventHandler , Event event) throws OperationAborted {
		if (getExecuteMethodDescriptor() != null) {
			getCodeDescriptor().invokeMethod(
					getExecuteMethodDescriptor(),
					new Object[] { eventDispatcher,
						eventHandler, event });
		}
	}
	
	@Override
	public boolean isAdapterForType(Object type) {
		return type instanceof Action;
	}

}
