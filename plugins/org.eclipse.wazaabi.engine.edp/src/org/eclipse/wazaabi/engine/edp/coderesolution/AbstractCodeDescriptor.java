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

package org.eclipse.wazaabi.engine.edp.coderesolution;

//TODO : need to find a way to manage different contructors

public abstract class AbstractCodeDescriptor {

	public static abstract class MethodDescriptor {

		private final String methodName;
		private final String[] parameterNames;

		private final Class<?>[] parameterTypes;

		private final Class<?> returnType;
		public MethodDescriptor(String methodName, String[] parameterNames,
				Class<?>[] parameterTypes, Class<?> returnType) {
			super();
			this.methodName = methodName;
			this.parameterNames = parameterNames;
			this.parameterTypes = parameterTypes;
			this.returnType = returnType;
		}

		public String getMethodName() {
			return methodName;
		}

		public String[] getParameterNames() {
			return parameterNames;
		}

		public Class<?>[] getParameterTypes() {
			return parameterTypes;
		}

		public Class<?> getReturnType() {
			return returnType;
		}

	}

	private String uri;

	public abstract MethodDescriptor getMethodDescriptor(String methodName,
			String[] parameterNames, Class<?>[] parameterTypes,
			Class<?> returnType);

	protected abstract AbstractMethodInvoker getMethodInvoker();

	public String getUri() {
		return uri;
	}

	public Object invokeMethod(MethodDescriptor methodDescriptor,
			Object[] parameterValues) {
		return getMethodInvoker().invokeMethod(this, methodDescriptor,
				parameterValues);
	}

	/**
	 * Returns true if the code underneath this <code>CodeDescriptor</code> has
	 * been instanciated or used once. A instanciated
	 * <code>CodeDescriptor</code> needs sometimes a specific dispose/release
	 * mechanism.
	 * 
	 * @return
	 */
	public abstract boolean isInstanciated();

	protected void setUri(String newUri) {
		this.uri = newUri;
	}
}
