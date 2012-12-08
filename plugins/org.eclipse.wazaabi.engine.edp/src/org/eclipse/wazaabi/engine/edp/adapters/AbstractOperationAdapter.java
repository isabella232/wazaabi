/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor.MethodDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractDeferredAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public abstract class AbstractOperationAdapter extends AbstractDeferredAdapter
		implements OperationAdapter {

	abstract public void trigger(EventDispatcher eventDispatcher,
			EventHandler eventHandler, Event event) throws OperationAborted;

	private List<MethodDescriptor> methodDescriptors = new ArrayList<MethodDescriptor>();

	private String codeLocatorBaseUri = null;

	protected MethodDescriptor getMethodDescriptor(int index) {
		if (index < methodDescriptors.size())
			return methodDescriptors.get(index);
		return null;
	}

	public void registerMethods(AbstractCodeDescriptor codeDescriptor) {
		if (getMethodSignatures() == null)
			return;
		for (MethodSignature methodSignature : getMethodSignatures())
			methodDescriptors.add(codeDescriptor.getMethodDescriptor(
					methodSignature.getMethodName(),
					methodSignature.getParameterNames(),
					methodSignature.getParameterTypes(),
					methodSignature.getReturnType()));
	}

	public String getCodeLocatorBaseUri() {
		return codeLocatorBaseUri;
	}

	public void setCodeLocatorBaseUri(String newBaseUri) {
		this.codeLocatorBaseUri = newBaseUri;
	}

}
