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

package org.eclipse.wazaabi.engine.edp.tests;

import java.beans.EventHandler;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.DeferredAdapter;

public class EventHandlerUtils {

	public static class UniqueMethodDescriptor {

		private final AbstractCodeDescriptor codeDescriptor;

		private final AbstractCodeDescriptor.MethodDescriptor methodDescriptor;

		public UniqueMethodDescriptor(AbstractCodeDescriptor codeDescriptor,
				AbstractCodeDescriptor.MethodDescriptor methodDescriptor) {
			super();
			this.codeDescriptor = codeDescriptor;
			this.methodDescriptor = methodDescriptor;
		}

		public AbstractCodeDescriptor getCodeDescriptor() {
			return codeDescriptor;
		}

		public AbstractCodeDescriptor.MethodDescriptor getMethodDescriptor() {
			return methodDescriptor;
		}

		public Object invokeMethod(Object parameterValues[]) {
			if (codeDescriptor != null && methodDescriptor != null)
				return codeDescriptor.invokeMethod(methodDescriptor,
						parameterValues);
			return null;
		}
	}

	/**
	 * Iterates on DeferredAdapter attached to this EventHandler and returns
	 * method descriptor corresponding to the given signature.
	 * 
	 * @param eventHandler
	 * @param methodName
	 * @param parameterTypes
	 * @param returnType
	 * @return
	 */
	public static UniqueMethodDescriptor getUniqueMethodDescriptor(
			EventHandler eventHandler, String methodName,
			Class<?>[] parameterTypes, Class<?> returnType) {
		if (eventHandler == null)
			return null;
		for (Adapter adapter : ((EObject)eventHandler).eAdapters())
			if (adapter instanceof DeferredAdapter) {
				AbstractCodeDescriptor codeDescriptor = ((DeferredAdapter) adapter)
						.getCodeDescriptor();
				if (codeDescriptor != null) {
					String[] parameters = null;
					AbstractCodeDescriptor.MethodDescriptor methodDescriptor = codeDescriptor
							.getMethodDescriptor(methodName, parameters, parameterTypes,
									returnType);
					if (methodDescriptor != null)
						return new UniqueMethodDescriptor(codeDescriptor,
								methodDescriptor);
				}
			}
		return null;
	}

}
